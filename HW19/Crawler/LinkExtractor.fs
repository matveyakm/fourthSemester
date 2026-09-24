// <copyright file="LinkExtractor.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Pure helpers for finding and resolving links in HTML content.
/// </summary>
module WebCrawler.LinkExtractor

open System
open System.Text.RegularExpressions

/// <summary>
/// Regular expression that matches an anchor tag and captures the value
/// of its href attribute. Quotes may be single or double.
/// </summary>
let private anchorHrefPattern = @"<a[^>]*\bhref\s*=\s*[""'](?<url>[^""' >]+)[""']"

/// <summary>
/// Precompiled, case-insensitive regular expression used to find anchors.
/// </summary>
let private anchorHrefRegex =
    Regex(anchorHrefPattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

/// <summary>
/// Returns true when the given address uses the http or https scheme.
/// </summary>
let private isHttpAddress (address: string) : bool =
    address.StartsWith("http://", StringComparison.OrdinalIgnoreCase)
    || address.StartsWith("https://", StringComparison.OrdinalIgnoreCase)

/// <summary>
/// Resolves a possibly relative link against the address of the page that holds it.
/// </summary>
/// <param name="pageUrl">The address of the page containing the link.</param>
/// <param name="href">The raw value of the href attribute.</param>
/// <returns>The absolute address of the link, if it could be resolved.</returns>
let private resolveLink (pageUrl: string) (href: string) : string option =
    let mutable baseUri: Uri = null
    let mutable resolved: Uri = null

    if Uri.TryCreate(pageUrl, UriKind.Absolute, &baseUri)
       && Uri.TryCreate(baseUri, href.Trim(), &resolved)
       && resolved.IsAbsoluteUri then
        Some resolved.AbsoluteUri
    else
        None

/// <summary>
/// Extracts the absolute addresses of all http and https links from HTML content.
/// Relative links are resolved against the address of the page.
/// </summary>
/// <param name="html">The HTML content to scan.</param>
/// <param name="pageUrl">The address of the page the HTML was downloaded from.</param>
/// <returns>The absolute addresses of the found links.</returns>
let extractLinks (html: string) (pageUrl: string) : string seq =
    anchorHrefRegex.Matches(html)
    |> Seq.cast<Match>
    |> Seq.choose (fun found ->
        found.Groups["url"].Value
        |> resolveLink pageUrl
        |> Option.filter isHttpAddress)