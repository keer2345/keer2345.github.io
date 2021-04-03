---
title: Spring Boot with Kotlin and JUnit5
date: 2021-03-31 23:15:21
tags: [spring, kotlin]
---

1. Introduction
1. Hello World
1. Project Structure
1. Data Layer
1. Data Source
1. Service Layer
1. Web Layer
1. GET Single Bank

<!-- more -->

- [Tutorial 01 Introduction](#tutorial-01-introduction)
  - [Introduction](#introduction)
  - [Create Project](#create-project)
- [Tutorial 02 Hello World](#tutorial-02-hello-world)
  - [Hello World](#hello-world)
  - [Create a REST Controller](#create-a-rest-controller)
- [Tutorial 3 Project Structure](#tutorial-3-project-structure)
- [Tutorial 4 Data Layer](#tutorial-4-data-layer)
- [Tutorial 5 Data Source](#tutorial-5-data-source)
- [Tutorial 6 Service Layer](#tutorial-6-service-layer)
- [Tutorial 7 Web Layer](#tutorial-7-web-layer)
- [Tutorial 8 GET Single Bank](#tutorial-8-get-single-bank)

# Tutorial 01 Introduction
## Introduction
Video: https://youtu.be/TJcshrJOnsE?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

- [Spring Project](https://spring.io/projects)

## Create Project

- https://start.spring.io/

Spring Initializr:
- Project: `Gradle`
- Language: `Kotlin`
- Spring Boot: `2.5.0 (SNAPSHOT)`
- Project Metadata
    - Group `tv.codealong.tutorials.springboot`
    - Artifact `thenewboston`
    - Name `thenewboston`
    - Description `Spring Boot Tutorial Series for TNB`
    - Package name `tv.codealong.tutorials.springboot.thenewboston`
    - Packaging: `Jar`
    - Java `11`
- Dependences:
    - Spring Web

# Tutorial 02 Hello World

Video: https://youtu.be/PqNgTV9enbE?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

## Hello World

```
touch src/main/resources/static/index.html
```
```html
<h1>Hello Spring Boot</h1>
```
`Run` this project in IntelliJ IDEA, and access http://127.0.0.1:8080 in Browers.

## Create a REST Controller
```
touch src/main/kotlin/tv/codealong/tutorials/springboot/thenewboston/HelloWorldController.kt
```
```kotlin
package tv.codealong.tutorials.springboot.thenewboston

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("api/hello")
class HelloWorldController {

    @GetMapping
    fun helloWorld(): String {
        return "Hello, this is a REST endpoint!"
    }
}
```

Access: http://127.0.0.1:8080/api/hello

# Tutorial 3 Project Structure

Video: https://youtu.be/j5G4QZy0kZk?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

Three files:
- *build.gradle.kts*
- *settings.gradle.kts*
- *gradle/wrapper/gradle-wrapper.properties*


**gradle-wrapper.properties**

Gradle Version in: *gradle/wrapper/gradle-wrapper.properties*

```properties
distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
distributionUrl=https\://services.gradle.org/distributions/gradle-6.8.3-bin.zip
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
```

- `./gradlew -v`
- `./gradlew clean`
- `./gradlew build`

```gradle
spring-boot-thenewboston > ./gradlew -v

------------------------------------------------------------
Gradle 6.8.3
------------------------------------------------------------

Build time:   2021-02-22 16:13:28 UTC
Revision:     9e26b4a9ebb910eaa1b8da8ff8575e514bc61c78

Kotlin:       1.4.20
Groovy:       2.5.12
Ant:          Apache Ant(TM) version 1.10.9 compiled on September 27 2020
JVM:          11.0.7 (Oracle Corporation 11.0.7+8-LTS)
OS:           Mac OS X 10.15.7 x86_64
```
```gradle
spring-boot-thenewboston > ./gradlew clean

BUILD SUCCESSFUL in 1s
1 actionable task: 1 executed
```
```gradle
spring-boot-thenewboston > ./gradlew build

BUILD SUCCESSFUL in 16s
8 actionable tasks: 8 executed
```

# Tutorial 4 Data Layer

Video: https://youtu.be/1D1iL824ssk?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

Api: https://thenewboston.com/bank-api/banks

```
touch src/main/kotlin/tv/codealong/tutorials/springboot/thenewboston/model/Bank.kt
```
```kotlin
package tv.codealong.tutorials.springboot.thenewboston.model

data class Bank(
    val accountNumber: String,
    val trust: Double,
    val transactionFee: Int
)
```

# Tutorial 5 Data Source
Video: https://youtu.be/VVrKvpqAmw8?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

```
touch src/main/kotlin/tv/codealong/tutorials/springboot/thenewboston/datasource/BankDataSource.kt
```
```kotlin
package tv.codealong.tutorials.springboot.thenewboston.datasource

import tv.codealong.tutorials.springboot.thenewboston.model.Bank

interface BankDataSource {
    fun retrieveBanks(): Collection<Bank>
}
```
```
touch src/main/kotlin/tv/codealong/tutorials/springboot/thenewboston/datasource/mock/MockBankDataSource.kt
```
```kotlin
package tv.codealong.tutorials.springboot.thenewboston.datasource.mock

import org.springframework.stereotype.Repository
import tv.codealong.tutorials.springboot.thenewboston.datasource.BankDataSource
import tv.codealong.tutorials.springboot.thenewboston.model.Bank

@Repository
class MockBankDataSource : BankDataSource {

    val banks = listOf(
        Bank("aaa", 2.0, 1),
        Bank("bbb", 3.0, 2),
        Bank("ccc", 4.0, 3)
    )

    override fun retrieveBanks(): Collection<Bank> = banks
}
```
```
touch src/test/kotlin/tv/codealong/tutorials/springboot/thenewboston/datasource/mock/MockBankDataSourceTest.kt
```
```kotlin
package tv.codealong.tutorials.springboot.thenewboston.datasource.mock

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

internal class MockBankDataSourceTest {
    private val mockDataSource: MockBankDataSource = MockBankDataSource()

    @Test
    fun `should provide a collection of banks`() {
        // when
        val banks = mockDataSource.retrieveBanks()

        // then
        assertThat(banks).isNotEmpty
        assertThat(banks.size).isGreaterThanOrEqualTo(3)
    }

    @Test
    fun `should provide some mock data`() {
        // when
        val banks = mockDataSource.retrieveBanks()

        // then
        assertThat(banks).allSatisfy { it.accountNumber.isNotBlank() }
        assertThat(banks).allMatch { it.accountNumber.isNotBlank() }
        assertThat(banks).anyMatch { it.trust != 0.0 }
        assertThat(banks).anyMatch { it.transactionFee != 0 }
    }
}
```

# Tutorial 6 Service Layer

Video: https://youtu.be/Icnb3zvya-w?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

- *build.gradle.kts*

Add `testImplementation("io.mockk:mockk:1.10.4")`:

``` gradle
dependencies {
    # ...
    testImplementation("io.mockk:mockk:1.10.4")
}
```
- *src/main/kotlin/tv/codealong/tutorials/springboot/thenewboston/service/BankService.kt*

```kotlin
package tv.codealong.tutorials.springboot.thenewboston.service

import io.mockk.mockk
import io.mockk.verify
import org.junit.jupiter.api.Test
import tv.codealong.tutorials.springboot.thenewboston.datasource.BankDataSource

internal class BankServiceTest {
    private val dataSource: BankDataSource = mockk(relaxed = true)
    private val bankService: BankService = BankService(dataSource)

    @Test
    fun `should call its data source to retrieve banks`() {
        // given
        // every { dataSource.retrieveBanks() } returns emptyList()

        // when
        val banks = bankService.getBanks()

        // then
        verify(exactly = 1) { dataSource.retrieveBanks() }
    }
}
```

- *src/test/kotlin/tv/codealong/tutorials/springboot/thenewboston/service/BankServiceTest.kt*

``` kotlin
package tv.codealong.tutorials.springboot.thenewboston.datasource.mock

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

internal class MockBankDataSourceTest {
    private val mockDataSource: MockBankDataSource = MockBankDataSource()

    @Test
    fun `should provide a collection of banks`() {
        // when
        val banks = mockDataSource.retrieveBanks()

        // then
        assertThat(banks).isNotEmpty
        assertThat(banks.size).isGreaterThanOrEqualTo(3)
    }

    @Test
    fun `should provide some mock data`() {
        // when
        val banks = mockDataSource.retrieveBanks()

        // then
        assertThat(banks).allSatisfy { it.accountNumber.isNotBlank() }
        assertThat(banks).allMatch { it.accountNumber.isNotBlank() }
        assertThat(banks).anyMatch { it.trust != 0.0 }
        assertThat(banks).anyMatch { it.transactionFee != 0 }
    }
}
```

# Tutorial 7 Web Layer

Video: https://youtu.be/2e72RHO8ORk?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr

- *src/main/kotlin/tv/codealong/tutorials/springboot/thenewboston/controller/BankController.kt*
``` kotlin
package tv.codealong.tutorials.springboot.thenewboston.controller

import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import tv.codealong.tutorials.springboot.thenewboston.model.Bank
import tv.codealong.tutorials.springboot.thenewboston.service.BankService

@RestController
@RequestMapping("/api/banks")
class BankController(private val service: BankService) {
    @GetMapping
    fun getBanks(): Collection<Bank> = service.getBanks()
}
```

- *src/test/kotlin/tv/codealong/tutorials/springboot/thenewboston/controller/BankControllerTest.kt*
``` kotlin
package tv.codealong.tutorials.springboot.thenewboston.controller

import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.http.MediaType
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.get

@SpringBootTest
@AutoConfigureMockMvc
internal class BankControllerTest {

    @Autowired
    lateinit var mockMvc: MockMvc

    @Test
    fun `should return all banks`() {
        mockMvc.get("/api/banks")
            .andDo { print() }
            .andExpect {
                status { isOk() }
                content { contentType(MediaType.APPLICATION_JSON) }
                jsonPath("$[0].accountNumber") { value("aaa") }
            }
    }
}
```

Access to  http://127.0.0.1:8080/api/banks
``` json
[
{
"accountNumber": "aaa",
"trust": 2,
"transactionFee": 1
},
{
"accountNumber": "bbb",
"trust": 3,
"transactionFee": 2
},
{
"accountNumber": "ccc",
"trust": 4,
"transactionFee": 3
}
]
```

# Tutorial 8 GET Single Bank
Video: https://youtu.be/jiv37i503v4?list=PL6gx4Cwl9DGDPsneZWaOFg0H2wsundyGr