package com.github.ephemient.aoc2023.buildsrc

import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.kotlin.dsl.apply
import org.gradle.kotlin.dsl.getValue
import org.jetbrains.kotlin.gradle.dsl.KotlinMultiplatformExtension

class KotlinMultiplatformBasePlugin : Plugin<Project> {
    override fun apply(target: Project) {
        target.pluginManager.withPlugin("org.jetbrains.kotlin.multiplatform") {
            val kotlin: KotlinMultiplatformExtension by target.extensions
            kotlin.applyDefaultHierarchyTemplate()
        }
    }
}

class KotlinMultiplatformJvmPlatformPlugin : Plugin<Project> {
    override fun apply(target: Project) {
        target.apply<KotlinMultiplatformBasePlugin>()
        target.pluginManager.withPlugin("org.jetbrains.kotlin.multiplatform") {
            val kotlin: KotlinMultiplatformExtension by target.extensions
            kotlin.jvm {
                compilations.all {
                    compilerOptions.configure {
                        freeCompilerArgs.add("-Xassertions=jvm")
                    }
                }
            }
        }
    }
}

class KotlinMultiplatformNativePlatformsPlugin : Plugin<Project> {
    override fun apply(target: Project) {
        target.apply<KotlinMultiplatformBasePlugin>()
        target.pluginManager.withPlugin("org.jetbrains.kotlin.multiplatform") {
            val kotlin: KotlinMultiplatformExtension by target.extensions
            kotlin.linuxX64()
            kotlin.macosX64()
            kotlin.macosArm64()
        }
    }
}
