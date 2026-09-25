// <copyright file="PageDownloader.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Tests for downloading a single web page and reporting its size.
/// </summary>
module WebCrawler.Tests.PageDownloaderTests

open System
open System.Net.Http
open System.Threading.Tasks
open NUnit.Framework
open FsUnit
open WebCrawler.PageDownloader

/// <summary>
/// Verifies that a known page is downloaded and its size is reported.
/// </summary>
[<Test>]
let ``downloadPageAsync reports the content and the size of a known page`` () : Task =
    task {
        let stub = new StubHttpMessageHandler(Map.ofList [ ("http://example.com/", "<html>Test</html>") ])
        use httpClient = new HttpClient(stub)

        let! outcome = downloadPageAsync httpClient "http://example.com/"

        match outcome with
        | Downloaded found ->
            found.Url |> should equal "http://example.com/"
            found.Content |> should equal "<html>Test</html>"
            found.Size |> should equal 17
        | DownloadFailed _ -> Assert.Fail "Expected a successful download"
    }

/// <summary>
/// Verifies that the size counts characters rather than bytes.
/// </summary>
[<Test>]
let ``downloadPageAsync counts characters, not bytes`` () : Task =
    task {
        let stub = new StubHttpMessageHandler(Map.ofList [ ("http://example.com/", "привет") ])
        use httpClient = new HttpClient(stub)

        let! outcome = downloadPageAsync httpClient "http://example.com/"

        match outcome with
        | Downloaded found -> found.Size |> should equal 6
        | DownloadFailed _ -> Assert.Fail "Expected a successful download"
    }

/// <summary>
/// Verifies that a missing page is reported as a failure with the address.
/// </summary>
[<Test>]
let ``downloadPageAsync reports a failure for a missing page`` () : Task =
    task {
        let stub = new StubHttpMessageHandler(Map.empty)
        use httpClient = new HttpClient(stub)

        let! outcome = downloadPageAsync httpClient "http://example.com/absent"

        match outcome with
        | DownloadFailed (url, message) ->
            url |> should equal "http://example.com/absent"
            message |> should not' (be Empty)
        | Downloaded _ -> Assert.Fail "Expected a download failure"
    }

/// <summary>
/// Verifies that a malformed address is reported as a failure rather than throwing.
/// </summary>
[<Test>]
let ``downloadPageAsync reports a failure for a malformed address`` () : Task =
    task {
        let stub = new StubHttpMessageHandler(Map.empty)
        use httpClient = new HttpClient(stub)

        let! outcome = downloadPageAsync httpClient "not a url at all"

        match outcome with
        | DownloadFailed (url, _) -> url |> should equal "not a url at all"
        | Downloaded _ -> Assert.Fail "Expected a download failure"
    }