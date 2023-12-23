enableFeaturePreview("TYPESAFE_PROJECT_ACCESSORS")

pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
    }

    plugins {
        resolutionStrategy {
            eachPlugin {
                if (requested.id.id == "kotlinx-atomicfu") {
                    useModule("org.jetbrains.kotlinx:atomicfu-gradle-plugin:${requested.version}")
                }
            }
        }
    }
}

gradle.afterProject {
    repositories {
        mavenCentral()
    }
}

rootProject.name = "aoc2023"
include("aoc2023-exe", "aoc2023-lib", "web", "web:common", "web:worker")
