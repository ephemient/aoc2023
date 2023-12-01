plugins {
    `kotlin-dsl`
}

group = "com.github.ephemient.aoc2023.build-logic"

gradlePlugin {
    plugins {
        create("KotlinMultiplatformDefaultHierarchyPlugin") {
            id = "com.github.ephemient.aoc2023.kotlin.multiplatform.base"
            implementationClass = "com.github.ephemient.aoc2023.buildsrc.KotlinMultiplatformBasePlugin"
        }
        create("KotlinMultiplatformJvmPlatformPlugin") {
            id = "com.github.ephemient.aoc2023.kotlin.multiplatform.jvm.platform"
            implementationClass = "com.github.ephemient.aoc2023.buildsrc.KotlinMultiplatformJvmPlatformPlugin"
        }
        create("KotlinMultiplatformNativePlatformsPlugin") {
            id = "com.github.ephemient.aoc2023.kotlin.multiplatform.native.platforms"
            implementationClass = "com.github.ephemient.aoc2023.buildsrc.KotlinMultiplatformNativePlatformsPlugin"
        }
    }
}

dependencies {
    implementation(kotlin("gradle-plugin", libs.versions.kotlin.get()))
    implementation(libs.detekt.plugin)
}
