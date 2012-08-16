// IDEA plugin
resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

addSbtPlugin("no.arktekk.sbt" % "aether-deploy" % "0.6")

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")
