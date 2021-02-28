---
title: Kotlin web frameworks of Kweb-core
date: 2021-02-24 21:25:35
tags: kotlin
---

Kotlin web frameworks:
- Spring Boot
- [Ktor](https://github.com/ktorio/ktor)
- [jooby](https://github.com/jooby-project/jooby)
- [kweb-core](https://github.com/kwebio/kweb-core)

We will introduce **Kweb-core** in this article.

<!-- more -->

# Introduction
## Motivation
Kweb’s goal is to eliminate this server/browser separation so that your webapp’s architecture is determined by the problem you’re solving, rather than the limitations of today’s tools.

# Getting Started
A common concern about this approach is that the user interface might feel sluggish if it is server driven. Kweb solves this problem by preloading instructions to the browser to be executed immediately on browser events without a server round-trip.

Kweb is built on the excellent *Ktor* framework, which handles HTTP, HTTPS, and WebSocket transport. You don’t need to know Ktor to use Kweb, but if you’ve already got a Ktor app you can [embed Kweb as a Feature](https://github.com/kwebio/kweb-demos/blob/master/ktorFeature/src/FeatureApp.kt).

## Features
- Allows the problem to determine your architecture, not the server/browser divide
- End-to-end Kotlin
- Keep the web page in sync with your back-end data in realtime, Kweb does all the plumbing for you
- Server-side HTML rendering with [rehydration](https://developers.google.com/web/updates/2019/02/rendering-on-the-web)
- Efficient instruction preloading to avoid unnecessary server communication
- Very lightweight, Kweb is less than 5,000 lines of code

## Relevant Links
- [Website](https://kweb.io/)
- [Source code](https://github.com/kwebio/kweb-core)
- [API documentation](http://dokka.kweb.io/kweb-core/)
- [Example projects](https://github.com/kwebio/kweb-demos)
- [Live Demo](http://demo.kweb.io:7659/)
- [Questions/Feedback/Bugs](https://github.com/kwebio/kweb-core/issues)

# Getting Started
