// <copyright file="Crawler.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// A breadth-first web crawler that downloads linked pages in parallel.
/// </summary>
module WebCrawler.Crawler

open System
open System.Net.Http
open System.Threading
open System.Threading.Tasks
open WebCrawler.LinkExtractor
open WebCrawler.PageDownloader

/// <summary>
/// Settings that control how the crawler behaves.
/// </summary>
/// <param name="MaxPages">The maximum total number of pages to download.</param>
/// <param name="ConcurrencyLimit">The maximum number of simultaneous downloads.</param>
/// <param name="RequestTimeout">How long to wait for one download before giving up.</param>
type CrawlOptions = {
    MaxPages: int
    ConcurrencyLimit: int
    RequestTimeout: TimeSpan
}

/// <summary>
/// The outcome of a whole crawl.
/// </summary>
/// <param name="Pages">The pages that were downloaded successfully.</param>
/// <param name="Errors">The addresses that could not be downloaded, with the error messages.</param>
type CrawlResult = {
    Pages: DownloadedPage list
    Errors: (string * string) list
}

/// <summary>
/// Default crawl settings: up to 100 pages, 10 simultaneous downloads, 30 second timeout.
/// </summary>
let defaultOptions : CrawlOptions = {
    MaxPages = 100
    ConcurrencyLimit = 10
    RequestTimeout = TimeSpan.FromSeconds 30.0
}

/// <summary>
/// Downloads a batch of pages while keeping the number of simultaneous
/// requests bounded by the concurrency limit.
/// </summary>
/// <param name="httpClient">The client used for the downloads.</param>
/// <param name="urls">The addresses to download.</param>
/// <param name="concurrencyLimit">The maximum number of simultaneous requests.</param>
/// <returns>The outcomes, in the same order as the given addresses.</returns>
let private downloadBatchAsync (httpClient) (urls) (concurrencyLimit) : Task<DownloadOutcome array> =
    task {
        use gate = new SemaphoreSlim(concurrencyLimit)

        let downloadWithLimit (url) =
            task {
                do! gate.WaitAsync()

                try
                    return! downloadPageAsync httpClient url
                finally
                    gate.Release() |> ignore
            }

        return! urls |> List.map downloadWithLimit |> Task.WhenAll
    }

/// <summary>
/// Tail-recursive breadth-first crawl over discovery levels. Every level
/// downloads its pages in parallel; the links found on that level form
/// the frontier of the next level.
/// </summary>
/// <param name="httpClient">The client used for the downloads.</param>
/// <param name="options">The crawl settings.</param>
/// <param name="visited">The addresses that have already been queued.</param>
/// <param name="frontier">The addresses to download in the current level.</param>
/// <param name="results">The successfully downloaded pages accumulated so far.</param>
/// <param name="errors">The download failures accumulated so far.</param>
/// <param name="budget">How many more downloads may still be attempted.</param>
let rec private crawlLevelsAsync
    (httpClient: HttpClient)
    (options: CrawlOptions)
    (visited: Set<string>)
    (frontier: string list)
    (results: DownloadedPage list)
    (errors: (string * string) list)
    (budget: int) : Task<CrawlResult> =

    if budget <= 0 || List.isEmpty frontier then
        task { return { Pages = results; Errors = errors } }
    else
        task {
            let batch = frontier |> List.truncate budget
            let batchSize = batch.Length
            let visitedWithBatch = Set.union visited (Set.ofList batch)

            let! outcomes = downloadBatchAsync httpClient batch options.ConcurrencyLimit

            let successes =
                outcomes
                |> Array.choose (function Downloaded found -> Some found | _ -> None)
                |> Array.toList

            let failures =
                outcomes
                |> Array.choose (function DownloadFailed (url, message) -> Some (url, message) | _ -> None)
                |> Array.toList

            let nextFrontier =
                successes
                |> List.collect (fun found -> extractLinks found.Content found.Url |> Seq.toList)
                |> List.filter (fun url -> not (visitedWithBatch.Contains url))
                |> List.distinct
                |> List.truncate (budget - batchSize)

            let! nextResult =
                crawlLevelsAsync httpClient options visitedWithBatch nextFrontier
                    (results @ successes) (errors @ failures) (budget - batchSize)

            return nextResult
        }

/// <summary>
/// Crawls the web starting from the given page and returns the pages that
/// were downloaded together with the failures. The starting page itself is
/// downloaded as well and the total number of downloads is bounded.
/// </summary>
/// <param name="httpClient">The client used to download the pages.</param>
/// <param name="startUrl">The address to start the crawl from.</param>
/// <param name="options">The crawl settings; None uses the defaults.</param>
/// <returns>The downloaded pages and the failures.</returns>
let crawlAsync (httpClient: HttpClient) (startUrl: string) (options: CrawlOptions option) : Task<CrawlResult> =
    let settings = defaultArg options defaultOptions
    crawlLevelsAsync httpClient settings Set.empty [ startUrl ] [] [] settings.MaxPages

/// <summary>
/// Prints the given crawl result, one line per page and per error.
/// </summary>
/// <param name="result">The crawl result to print.</param>
let printCrawlResult (result: CrawlResult) : unit =
    for page in result.Pages do
        printfn "%s — %d символов" page.Url page.Size

    for url, message in result.Errors do
        eprintfn "Failed to download %s: %s" url message

/// <summary>
/// Crawls the web starting from the given page and prints the size of
/// every downloaded page in the form "address — N символов".
/// </summary>
/// <param name="startUrl">The address to start the crawl from.</param>
/// <param name="options">The crawl settings; None uses the defaults.</param>
/// <returns>A task that completes when the crawl and the printing are finished.</returns>
let crawlAndPrintAsync (startUrl: string) (options: CrawlOptions option) : Task =
    task {
        let settings = defaultArg options defaultOptions
        use httpClient = new HttpClient(Timeout = settings.RequestTimeout)

        let! result = crawlAsync httpClient startUrl options
        printCrawlResult result
    }