plugins {
    alias(libs.plugins.kotlin.plugin.allopen) apply false
    alias(libs.plugins.kotlinx.benchmark) apply false
    alias(libs.plugins.dependency.updates)
    id("com.github.ephemient.aoc2023.detekt")
}

tasks.detekt {
    setSource(files().apply { from(layout.projectDirectory.asFileTree.matching { include("*.kts") }) })
}

tasks.dependencyUpdates {
    revision = "release"
    gradleReleaseChannel = "current"
}
