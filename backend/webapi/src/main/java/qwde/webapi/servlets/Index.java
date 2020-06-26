package qwde.backend.webapi.servlets;

import com.google.common.collect.ImmutableMap;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.reactivex.Single;
import qwde.backend.webapi.plotly.PageRenderer;

@Controller(value = "/", produces = MediaType.TEXT_HTML)
public final class Index {
    @Get("/")
    public Single<String> doGet() {
        return Single.just(PageRenderer.renderPage("index.ftl", ImmutableMap.of()));
    }
}
