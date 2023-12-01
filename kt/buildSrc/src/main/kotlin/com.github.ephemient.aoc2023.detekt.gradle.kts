import io.gitlab.arturbosch.detekt.Detekt

plugins {
    `lifecycle-base`
    id("io.gitlab.arturbosch.detekt")
}

dependencies {
    versionCatalogs.named("libs").findBundle("detekt-plugins").ifPresent {
        detektPlugins(it)
    }
}

detekt {
    config.from(rootProject.file("detekt.yml"))
    buildUponDefaultConfig = true
    autoCorrect = System.getenv("CI").isNullOrEmpty()
}

val detektAll by tasks.registering {
    group = LifecycleBasePlugin.VERIFICATION_GROUP
    dependsOn(tasks.withType<Detekt>())
}

tasks.check {
    dependsOn(detektAll)
}
