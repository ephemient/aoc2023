plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.jvm.platform")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.native.platforms")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
}

kotlin {
    js {
        browser()
        nodejs()
    }

    sourceSets {
        commonTest {
            dependencies {
                implementation(kotlin("test"))
            }
        }

        jvmTest {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }
    }
}

tasks.jvmTest {
    useJUnitPlatform()
}
