import org.jetbrains.kotlin.gradle.targets.js.dsl.KotlinJsBinaryMode
import org.jetbrains.kotlin.gradle.targets.js.ir.JsIrBinary

plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.wasm.platform")
}

kotlin {
    js {
        browser {
            commonWebpackConfig {
                outputFileName = "aoc2023-web.js"
                experiments += "topLevelAwait"
            }
        }
        binaries.executable()
    }

    wasmJs {
        d8()
        for (binary in binaries.library().filterIsInstance<JsIrBinary>()) {
            if (binary.mode != KotlinJsBinaryMode.PRODUCTION) continue
            sourceSets.jsMain {
                resources.srcDir(binary.linkSyncTask.flatMap { it.destinationDirectory })
            }
        }
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2023Lib)
            }
        }

        jsMain {
            dependencies {
                implementation(libs.kotlin.wrappers.browser)
                implementation(libs.kotlinx.coroutines)
                implementation(libs.kotlinx.html)
            }
        }
    }
}
