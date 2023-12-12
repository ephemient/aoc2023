plugins {
    `kotlin-dsl`
}

group = "com.github.ephemient.aoc2023.build-logic"

gradlePlugin {
    plugins {
        create("KotlinMultiplatformJvmPlatformPlugin") {
            id = "com.github.ephemient.aoc2023.kotlin.multiplatform.jvm.platform"
            implementationClass = "com.github.ephemient.aoc2023.buildsrc.KotlinMultiplatformJvmPlatformPlugin"
        }
        create("KotlinMultiplatformNativePlatformsPlugin") {
            id = "com.github.ephemient.aoc2023.kotlin.multiplatform.native.platforms"
            implementationClass = "com.github.ephemient.aoc2023.buildsrc.KotlinMultiplatformNativePlatformsPlugin"
        }
        create("KotlinMultiplatformJsPlatformPlugin") {
            id = "com.github.ephemient.aoc2023.kotlin.multiplatform.js.platform"
            implementationClass = "com.github.ephemient.aoc2023.buildsrc.KotlinMultiplatformJsPlatformPlugin"
        }
    }
}

dependencies {
    implementation(kotlin("gradle-plugin", libs.versions.kotlin.get()))
    implementation(libs.detekt.plugin)
}
