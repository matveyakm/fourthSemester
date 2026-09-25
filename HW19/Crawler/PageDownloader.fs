// <copyright file="PageDownloader.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Downloads a single web page and reports the outcome of the attempt.
/// </summary>
module WebCrawler.PageDownloader

open System
open System.Net.Http
open System.Threading.Tasks

/// <summary>
/// A web page whose content has been downloaded successfully.
/// </summary>
/// <param name="Url">The address of the page.</param>
/// <param name="Content">The downloaded HTML content.</param>
/// <param name="Size">The number of characters in the content.</param>
type DownloadedPage = {
    Url: string
    Content: string
    Size: int
}

/// <summary>
/// Describes the result of an attempt to download a single web page.
/// </summary>
type DownloadOutcome =
    /// <summary>The page was downloaded successfully.</summary>
    | Downloaded of DownloadedPage
    /// <summary>The page could not be downloaded; the message explains why.</summary>
    | DownloadFailed of Url: string * Message: string

/// <summary>
/// Downloads one page and reports the outcome. Transport errors do not
/// escape; they are returned as a DownloadFailed value instead.
/// </summary>
/// <param name="httpClient">The client used for the download.</param>
/// <param name="url">The address of the page.</param>
/// <returns>An outcome describing the download.</returns>
let downloadPageAsync (httpClient: HttpClient) (url: string) : Task<DownloadOutcome> =
    task {
        try
            let! content = httpClient.GetStringAsync(url)
            return Downloaded { Url = url; Content = content; Size = content.Length }
        with
        | ex -> return DownloadFailed (url, ex.Message)
    }