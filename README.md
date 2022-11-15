[![Project stage][Stage]][Stage-Page]
![Build](https://github.com/zio/izumi-reflect/workflows/Build/badge.svg)
[![javadoc](https://javadoc.io/badge2/dev.zio/izumi-reflect_2.13/javadoc.svg)](https://javadoc.io/doc/dev.zio/izumi-reflect_2.13)

---

<p align="center">
  <a href="https://www.buymeacoffee.com/7mind"><img src="https://bmc-cdn.nyc3.digitaloceanspaces.com/BMC-button-images/custom_images/orange_img.png" alt="Izumi"/></a>
</p>

---

# Documentation
[izumi-reflect Homepage](https://zio.dev/izumi-reflect/)

## Credits

`izumi-reflect` has been created by [Septimal Mind](https://7mind.io) to power [Izumi Project](https://github.com/7mind/izumi),
as a replacement for `TypeTag` in reaction to a lack of confirmed information about the future of `scala-reflect`/`TypeTag` in Scala 3 ([Motivation](https://blog.7mind.io/lightweight-reflection.html)), and donated to ZIO.

<p align="center">
  <a href="https://izumi.7mind.io/">
  <img width="40%" src="https://github.com/7mind/izumi/blob/develop/doc/microsite/src/main/tut/media/izumi-logo-full-purple.png?raw=true" alt="Izumi"/>
  </a>
</p>


[Stage]: https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages

# See also

## [`gzoller/scala-reflection`](https://github.com/gzoller/scala-reflection)

* Scala 3 only
* No support for subtype checks
* Requires compiler plugin
* Type lambdas are not supported
* Preserves field information

## [`airframe-surface`](https://wvlet.org/airframe/docs/airframe-surface)

* Scala 2 and Scala 3
* No support for subtype checks
* Preserves field information
