package qwde.webapi.servlets;

import com.google.common.collect.ImmutableMap;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

@Controller(value = "/random", produces = MediaType.TEXT_JSON)
public final class Random {
    @Get("/")
    public HttpResponse<ImmutableMap<String, List<Double>>> doGet() {
        return HttpResponse.ok(ImmutableMap.of("numbers", DoubleStream
                .generate(ThreadLocalRandom.current()::nextDouble)
                .limit(10)
                .map(x -> Math.ceil(x * 10))
                .boxed()
                .collect(Collectors.toList())));
    }
}
