// <copyright file="StubHttpMessageHandler.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

namespace WebCrawler.Tests

open System
open System.Net
open System.Net.Http
open System.Threading
open System.Threading.Tasks

/// <summary>
/// HttpMessageHandler for tests that serves prepared HTML for known
/// addresses and an empty 404 response for all other addresses.
/// </summary>
/// <param name="pages">The map of addresses to the HTML they should return.</param>
/// <param name="delay">An optional artificial delay applied to every request.</param>
type StubHttpMessageHandler(pages: Map<string, string>, delay: TimeSpan) =
    inherit HttpMessageHandler()

    /// <summary>
    /// Creates a handler that serves the given pages without an artificial delay.
    /// </summary>
    /// <param name="pages">The map of addresses to the HTML they should return.</param>
    new(pages: Map<string, string>) = new StubHttpMessageHandler(pages, TimeSpan.Zero)

    /// <summary>Responds to a request with the prepared HTML or a 404.</summary>
    /// <param name="request">The incoming request.</param>
    /// <param name="cancellationToken">Cancellation token of the request.</param>
    /// <returns>The prepared response.</returns>
    override _.SendAsync(request: HttpRequestMessage, cancellationToken: CancellationToken) : Task<HttpResponseMessage> =
        task {
            if delay > TimeSpan.Zero && not cancellationToken.IsCancellationRequested then
                do! Task.Delay(delay, cancellationToken)

            let response = new HttpResponseMessage()
            let url = request.RequestUri.AbsoluteUri

            match pages.TryFind url with
            | Some html ->
                response.Content <- new StringContent(html)
            | None ->
                response.StatusCode <- HttpStatusCode.NotFound
                response.Content <- new StringContent("Not found")

            return response
        }