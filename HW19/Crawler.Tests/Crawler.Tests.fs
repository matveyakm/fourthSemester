// <copyright file="Crawler.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Integration tests for the crawler built on a stubbed HTTP stack.
/// </summary>
module WebCrawler.Tests.CrawlerTests

open System
open System.Net
open System.Net.Http
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open FsUnit
open WebCrawler.Crawler

/// <summary>
/// HttpMessageHandler used to verify that several pages are downloaded
/// in parallel: it delays every response and records the maximum number
/// of requests that were in flight at the same time.
/// </summary>
/// <param name="pages">The map of addresses to the HTML they should return.</param>
/// <param name="delay">The artificial delay applied to every request.</param>
/// <param name="observed">The reference that receives the maximum concurrency.</param>
type ConcurrentRequestsHandler(pages: Map<string, string>, delay: TimeSpan, observed: int ref) =
    inherit HttpMessageHandler()

    let gate = obj()
    let active = System.Collections.Generic.HashSet<string>()

    /// <summary>Delays the response and tracks the number of in-flight requests.</summary>
    /// <param name="request">The incoming request.</param>
    /// <param name="cancellationToken">Cancellation token of the request.</param>
    /// <returns>The prepared response.</returns>
    override _.SendAsync(request: HttpRequestMessage, cancellationToken: CancellationToken) : Task<HttpResponseMessage> =
        task {
            let url = request.RequestUri.AbsoluteUri

            lock gate (fun () ->
                active.Add(url) |> ignore
                if active.Count > !observed then observed := active.Count)

            try
                if delay > TimeSpan.Zero && not cancellationToken.IsCancellationRequested then
                    do! Task.Delay(delay, cancellationToken)

                let response = new HttpResponseMessage()

                match pages.TryFind url with
                | Some html -> response.Content <- new StringContent(html)
                | None ->
                    response.StatusCode <- HttpStatusCode.NotFound
                    response.Content <- new StringContent("Not found")

                return response
            finally
                lock gate (fun () -> active.Remove url |> ignore)
        }

/// <summary>
/// Verifies that the start page and every page linked from it are downloaded.
/// </summary>
[<Test>]
let ``crawlAsync downloads the start page and the pages linked from it`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/a\">a</a> <a href=\"http://example.com/b\">b</a>"
                "http://example.com/a", "<p>page a</p>"
                "http://example.com/b", "<p>page b</p>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages
        |> List.map (fun found -> found.Url)
        |> Set.ofList
        |> should equal (Set.ofList [ "http://example.com/"; "http://example.com/a"; "http://example.com/b" ])

        result.Errors |> should be Empty
    }

/// <summary>
/// Verifies that the reported size is the number of characters in the content.
/// </summary>
[<Test>]
let ``crawlAsync reports the number of characters of every page`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/a\">a</a> <a href=\"http://example.com/b\">b</a>"
                "http://example.com/a", "<p>page a</p>"
                "http://example.com/b", "<p>page b</p>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages
        |> List.filter (fun found -> found.Url <> "http://example.com/")
        |> List.map (fun found -> found.Url, found.Size)
        |> should equal [ ("http://example.com/a", 13); ("http://example.com/b", 13) ]
    }

/// <summary>
/// Verifies that links found on linked pages are followed as well.
/// </summary>
[<Test>]
let ``crawlAsync follows links on linked pages`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/inner\">inner</a>"
                "http://example.com/inner", "<a href=\"http://example.com/deep\">deep</a>"
                "http://example.com/deep", "<p>deep page</p>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages
        |> List.map (fun found -> found.Url)
        |> Set.ofList
        |> should equal (Set.ofList [ "http://example.com/"; "http://example.com/inner"; "http://example.com/deep" ])
    }

/// <summary>
/// Verifies that relative links are resolved before being downloaded.
/// </summary>
[<Test>]
let ``crawlAsync resolves relative links`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"/docs\">docs</a>"
                "http://example.com/docs", "<p>docs</p>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages
        |> List.map (fun found -> found.Url)
        |> should contain "http://example.com/docs"
    }

/// <summary>
/// Verifies that links with non-http schemes are not downloaded.
/// </summary>
[<Test>]
let ``crawlAsync ignores links with non-http schemes`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/",
                "<a href=\"ftp://files.example.net/\">ftp</a> <a href=\"mailto:info@example.com\">mail</a>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages |> List.map (fun found -> found.Url) |> should equal [ "http://example.com/" ]
        result.Errors |> should be Empty
    }

/// <summary>
/// Verifies that each address is downloaded at most once and cycles terminate.
/// </summary>
[<Test>]
let ``crawlAsync visits each address only once`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/target\">target</a>"
                "http://example.com/target",
                "<a href=\"http://example.com/\">back</a> <a href=\"http://example.com/target\">again</a>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages |> List.map (fun found -> found.Url) |> fun urls ->
            urls |> List.distinct |> should equal urls
        result.Pages |> List.length |> should equal 2
    }

/// <summary>
/// Verifies that the total number of downloads never exceeds the limit.
/// </summary>
[<Test>]
let ``crawlAsync does not download more pages than allowed`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/a\">a</a>"
                "http://example.com/a", "<a href=\"http://example.com/b\">b</a>"
                "http://example.com/b", "<a href=\"http://example.com/c\">c</a>"
                "http://example.com/c", "<a href=\"http://example.com/d\">d</a>"
                "http://example.com/d", "<p>d</p>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 3; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages |> List.length |> should equal 3
        result.Pages |> List.map (fun found -> found.Url) |> should equal
            [ "http://example.com/"; "http://example.com/a"; "http://example.com/b" ]
    }

/// <summary>
/// Verifies that a page that cannot be downloaded is reported as an error.
/// </summary>
[<Test>]
let ``crawlAsync reports pages that cannot be downloaded`` () : Task =
    task {
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/broken\">broken</a>"
            ]

        use httpClient = new HttpClient(new StubHttpMessageHandler(stubPages))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 3 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages |> List.map (fun found -> found.Url) |> should equal [ "http://example.com/" ]
        result.Errors |> List.map fst |> should equal [ "http://example.com/broken" ]
    }

/// <summary>
/// Verifies that the pages of one discovery level are downloaded in parallel.
/// </summary>
[<Test>]
let ``crawlAsync downloads the pages of a level in parallel`` () : Task =
    task {
        let observed = ref 0
        let stubPages =
            Map.ofList [
                "http://example.com/", "<a href=\"http://example.com/one\">1</a> <a href=\"http://example.com/two\">2</a>"
                "http://example.com/one", "<p>one</p>"
                "http://example.com/two", "<p>two</p>"
            ]

        use httpClient = new HttpClient(new ConcurrentRequestsHandler(stubPages, TimeSpan.FromMilliseconds 200.0, observed))
        let options = { defaultOptions with MaxPages = 10; ConcurrencyLimit = 10 }

        let! result = crawlAsync httpClient "http://example.com/" (Some options)

        result.Pages |> List.length |> should equal 3
        !observed |> should be (greaterThanOrEqualTo 2)
    }

/// <summary>
/// Verifies that every downloaded page is printed along with its size.
/// </summary>
[<Test>]
let ``printCrawlResult prints the address and the size of every page`` () =
    let result = {
        Pages = [ { Url = "http://example.com/page"; Content = "<p>hi</p>"; Size = 42 } ]
        Errors = []
    }

    let previous = Console.Out
    let output = new System.IO.StringWriter()
    Console.SetOut(output)

    try
        printCrawlResult result
    finally
        Console.SetOut(previous)

    output.ToString() |> should equal ("http://example.com/page — 42 символов" + Environment.NewLine)

/// <summary>
/// Verifies that failed pages are described on the error stream.
/// </summary>
[<Test>]
let ``printCrawlResult reports failed pages on the error stream`` () =
    let result = {
        Pages = []
        Errors = [ ("http://example.com/broken", "404 Not Found") ]
    }

    let previous = Console.Error
    let output = new System.IO.StringWriter()
    Console.SetError(output)

    try
        printCrawlResult result
    finally
        Console.SetError(previous)

    output.ToString() |> should equal ("Failed to download http://example.com/broken: 404 Not Found" + Environment.NewLine)

/// <summary>
/// Verifies the defaults used when no options are provided.
/// </summary>
[<Test>]
let ``defaultOptions provides sane defaults`` () =
    defaultOptions.MaxPages |> should equal 100
    defaultOptions.ConcurrencyLimit |> should equal 10
    defaultOptions.RequestTimeout |> should equal (TimeSpan.FromSeconds 30.0)