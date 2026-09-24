// <copyright file="LinkExtractor.Tests.fs" company="matveyakm">
// Copyright (c) matveyakm. All rights reserved.
// </copyright>

/// <summary>
/// Tests for the extraction and resolution of links from HTML content.
/// </summary>
module WebCrawler.Tests.LinkExtractorTests

open NUnit.Framework
open FsUnit
open WebCrawler.LinkExtractor

/// <summary>
/// Verifies that a plain absolute http link is found.
/// </summary>
[<Test>]
let ``extractLinks finds an absolute http link`` () =
    let links = extractLinks "<a href=\"http://example.com/\">home</a>" "http://example.com/" |> Seq.toList

    links |> should equal [ "http://example.com/" ]

/// <summary>
/// Verifies that an absolute https link is found as well.
/// </summary>
[<Test>]
let ``extractLinks finds an absolute https link`` () =
    let links = extractLinks "<a href=\"https://example.com/page\">page</a>" "http://example.com/" |> Seq.toList

    links |> should equal [ "https://example.com/page" ]

/// <summary>
/// Verifies that a relative link is resolved against the address of the page.
/// </summary>
[<Test>]
let ``extractLinks resolves a relative link against the page address`` () =
    let links = extractLinks "<a href=\"/about\">about</a>" "http://example.com/blog/post" |> Seq.toList

    links |> should equal [ "http://example.com/about" ]

/// <summary>
/// Verifies that single-quoted href values are recognized.
/// </summary>
[<Test>]
let ``extractLinks handles single-quoted href values`` () =
    let links = extractLinks "<a href='http://example.com/shop'>shop</a>" "http://example.com/" |> Seq.toList

    links |> should equal [ "http://example.com/shop" ]

/// <summary>
/// Verifies that the tag and the attribute are matched case-insensitively.
/// </summary>
[<Test>]
let ``extractLinks matches an uppercase anchor and attribute`` () =
    let links = extractLinks "<A HREF=\"http://example.com/next\">next</A>" "http://example.com/" |> Seq.toList

    links |> should equal [ "http://example.com/next" ]

/// <summary>
/// Verifies that an anchor carrying other attributes before the href is handled.
/// </summary>
[<Test>]
let ``extractLinks handles attributes placed before the href`` () =
    let html = "<a class=\"nav\" id=\"main\" href=\"http://example.com/next\">next</a>"
    let links = extractLinks html "http://example.com/" |> Seq.toList

    links |> should equal [ "http://example.com/next" ]

/// <summary>
/// Verifies that links with non-http schemes are ignored.
/// </summary>
[<Test>]
let ``extractLinks ignores links with non-http schemes`` () =
    let html =
        "<a href=\"ftp://files.example.net/\">ftp</a> \
         <a href=\"mailto:info@example.com\">mail</a> \
         <a href=\"tel:+123456\">tel</a> \
         <a href=\"javascript:void(0)\">js</a>"
    let links = extractLinks html "http://example.com/" |> Seq.toList

    links |> should be Empty

/// <summary>
/// Verifies that HTML without anchors yields no links.
/// </summary>
[<Test>]
let ``extractLinks returns nothing for HTML without anchors`` () =
    let links = extractLinks "<html><body>no anchors here</body></html>" "http://example.com/" |> Seq.toList

    links |> should be Empty

/// <summary>
/// Verifies that all links of a page are returned in order.
/// </summary>
[<Test>]
let ``extractLinks returns every found link`` () =
    let html = "<a href=\"http://a.example/\">a</a><a href=\"http://b.example/\">b</a><a href=\"http://c.example/\">c</a>"
    let links = extractLinks html "http://example.com/" |> Seq.toList

    links |> should equal [ "http://a.example/"; "http://b.example/"; "http://c.example/" ]

/// <summary>
/// Verifies that an anchor without a usable address is skipped.
/// </summary>
[<Test>]
let ``extractLinks skips an anchor without a usable address`` () =
    let html = "<a href=\"\">empty</a><a>no href</a>"
    let links = extractLinks html "http://example.com/" |> Seq.toList

    links |> should be Empty