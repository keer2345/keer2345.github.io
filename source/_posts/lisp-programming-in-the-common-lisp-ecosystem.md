---
title: Programming in the Common Lisp Ecosystem
date: 2021-03-13 20:24:38
tags: lisp
---
I've really enjoyed working with a few projects and tools. I'll probably write more about these individually in the near future, but in brief:

<!--more-->

- [chanl](https://github.com/zkat/chanl) provides. As a current/recovering Go programmer, this library is very familiar and great to have. In some ways, the API provides a bit more introspection, and flexibility that I've always wanted in Go.
- [lake](https://github.com/takagi/lake) is a buildsystem tool, in the tradition of make, but with a few additional great features, like target namespacing, a clear definition between "file targets" and "task targets," as well as support for SSH operations, which makes it a reasonable replacement for things like fabric, and other basic deployment tools.
- [cl-docutils](https://github.com/willijar/cl-docutils) provides the basis for a document processing system. I'm particularly partial because I've been using the python (reference) implementation for years, but the implementation is really quite good and quite easy to extend.
- roswell is really great for getting started with CL, and also for making it possible to test library code against different implementations and versions of the language. I'm a touch iffy on using it to install packages into it's own directory, but it's pretty great.
- ASDF is the "buildsystem" component of CL, comparable to setuptools in python, and it (particularly the latest versions,) is really great. I like the ability to produce binaries directly from asdf, and the "package-inferred" is a great addition (basically, giving python-style automatic package discovery.)
- There's a full Apache Thrift implementation. While I'm not presently working on anything that would require a legit RPC protocol, being able to integrate CL components into larger ecosystem, having the option is useful.
- [Hunchensocket](https://github.com/joaotavora/hunchensocket) adds websockets! Web sockets are a weird little corner of any stack, but it's nice to be able to have the option of being able to do this kind of programming. Also CL seems like a really good platform to do
- [make-hash](https://github.com/genovese/make-hash) makes constructing hash tables easier, which is sort of needlessly gawky otherwise.
- [ceramic](https://github.com/ceramic/ceramic) provides bridges between CL and Electron for delivering desktop applications based on web technologies in CL.

I kept thinking that there wouldn't be good examples of various things, (there's a Kafka driver! there's support for various other Apache ecosystem components,) but there are, and that's great. There's gaps, of course, but fewer, I think, than you'd expect.
