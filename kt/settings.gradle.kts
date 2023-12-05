enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }
}

gradle.afterProject {
    repositories {
        mavenCentral()
    }
}

rootProject.name = "aoc2023"
include("aoc2023-exe", "aoc2023-lib", "web", "web:common", "web:wasm", "web:worker")
