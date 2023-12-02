plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
}

kotlin {
    js {
        useCommonJs()
        browser {
            commonWebpackConfig {
                outputFileName = "script.js"
            }
        }
        binaries.executable()
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2023Lib)
                implementation(libs.kotlinx.coroutines)
            }
        }

        jsMain {
            dependencies {
                implementation(libs.kotlin.wrappers.browser)
                implementation(libs.kotlinx.html)
            }
        }
    }
}
