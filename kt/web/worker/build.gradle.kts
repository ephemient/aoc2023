plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
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
                implementation(libs.kotlinx.coroutines)
            }
        }

        jsMain {
            dependencies {
                implementation(libs.kotlin.wrappers.browser)
            }
        }
    }
}

artifacts {
    add("default", layout.buildDirectory.dir("dist/js/productionExecutable")) {
        builtBy("jsBrowserDistribution")
    }
}
