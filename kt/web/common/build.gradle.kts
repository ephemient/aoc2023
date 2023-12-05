plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
}

kotlin {
    js {
        browser()
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
            }
        }
    }
}
