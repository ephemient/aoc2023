import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTarget
import org.jetbrains.kotlin.gradle.targets.jvm.KotlinJvmTarget

plugins {
    application
    kotlin("multiplatform")
    kotlin("plugin.allopen")
    alias(libs.plugins.kotlinx.benchmark)
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.jvm.platform")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.native.platforms")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform")
}

application {
    mainClass = "com.github.ephemient.aoc2023.exe.Main"
}

kotlin {
    jvm {
        withJava()
    }

    js {
        useCommonJs()
        nodejs()
        binaries.executable()
    }

    targets.withType<KotlinJvmTarget> {
        mainRun {
            mainClass = application.mainClass
        }
    }

    targets.withType<KotlinNativeTarget> {
        binaries.executable {
            entryPoint = "com.github.ephemient.aoc2023.exe.main"
        }
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2023Lib)
                implementation(libs.kotlinx.coroutines)
            }
        }

        commonTest {
            dependencies {
                implementation(libs.kotlinx.benchmark)
            }
        }

        nativeMain {
            dependencies {
                implementation(libs.okio)
            }
        }

        jsMain {
            dependencies {
                implementation(libs.kotlin.wrappers.node)
            }
        }
    }
}

allOpen {
    annotation("org.openjdk.jmh.annotations.State")
}

benchmark {
    targets {
        register("jvmTest")
    }

    configurations {
        getByName("main") {
            warmups = 1
            iterationTime = 1
            outputTimeUnit = "SECONDS"
        }
    }
}