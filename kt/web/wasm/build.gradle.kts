plugins {
    kotlin("multiplatform")
    id("com.github.ephemient.aoc2023.detekt")
    id("com.github.ephemient.aoc2023.kotlin.multiplatform.wasm.platform")
}

kotlin {
    wasmJs {
        d8()
        binaries.library()
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2023Lib)
            }
        }
    }
}

artifacts {
    add("default", layout.buildDirectory.dir("dist/wasmJs/productionLibrary")) {
        builtBy("wasmJsD8ProductionLibraryDistribution")
    }
}
