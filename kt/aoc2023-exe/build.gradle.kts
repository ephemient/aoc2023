import io.gitlab.arturbosch.detekt.Detekt
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
                implementation(libs.kotlinx.benchmark)
                implementation(libs.kotlinx.coroutines)
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

                // Benchmark generation failure without these
                implementation("org.jetbrains.kotlin:kotlinx-atomicfu-runtime:${this@kotlin.coreLibrariesVersion}")
                implementation("io.github.turansky.seskar:seskar-core:2.22.0")
                implementation("io.github.turansky.seskar:seskar-react:2.42.0")
            }
        }
    }
}

allOpen {
    annotation("org.openjdk.jmh.annotations.State")
}

benchmark {
    targets {
        register("jvm")
        register("linuxX64")
        register("macosX64")
        register("macosArm64")
        register("js")
    }

    configurations {
        getByName("main") {
            warmups = 1
            iterationTime = 1
            iterationTimeUnit = "s"
            mode = "avgt"
            outputTimeUnit = "us"
            val benchmarkInclude: String? by project
            val benchmarkExclude: String? by project
            benchmarkInclude?.let(::include)
            benchmarkExclude?.let(::exclude)
        }
    }
}

tasks.withType<Detekt>()
    .matching { it.name.endsWith("Benchmark") }
    .configureEach { isEnabled = false }
