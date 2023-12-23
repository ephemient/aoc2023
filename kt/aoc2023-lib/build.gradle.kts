plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.jvm.platform")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.native.platforms")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
    alias(libs.plugins.kotlinx.atomicfu)
}

atomicfu {
    jvmVariant = "VH"
}

kotlin {
    js {
        browser()
        nodejs()
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(libs.kotlinx.atomicfu)
                implementation(libs.kotlinx.coroutines)
            }
        }

        commonTest {
            dependencies {
                implementation(kotlin("test"))
                implementation(libs.kotlinx.coroutines.test)
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
