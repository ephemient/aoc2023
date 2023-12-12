plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
}

val workerBinary by configurations.creating
val syncWorkerBinary by tasks.registering(Sync::class) {
    into(layout.buildDirectory.dir("resources/$name/worker"))
    from(workerBinary)
}

kotlin {
    js {
        browser()
        binaries.executable()
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2023Lib)
                implementation(projects.web.common)
            }
        }

        jsMain {
            dependencies {
                implementation(libs.kotlin.wrappers.browser)
                implementation(libs.kotlinx.coroutines)
                implementation(libs.kotlinx.html)
            }

            resources.srcDir(syncWorkerBinary.map { it.destinationDir.parentFile })
        }
    }
}

dependencies {
    workerBinary(projects.web.worker) { targetConfiguration = "default" }
}
