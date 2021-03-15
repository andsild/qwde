package qwde.backend.webapi.servlets;

import com.google.common.collect.ImmutableMap;

import io.micronaut.cache.annotation.CacheConfig;
import io.micronaut.cache.annotation.Cacheable;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import qwde.backend.dataprovider.db.StockDB;

import java.sql.SQLException;
import java.util.Set;
import java.util.HashSet;

@Controller(value = "/tickers", produces = MediaType.APPLICATION_JSON)
@CacheConfig("tickers")
public class Tickers {
    public Set<String> tickers = new HashSet<>();

    @Get()
    public HttpResponse<?> doGet() throws SQLException {
        Set<String> data = getTickers();
        return HttpResponse.ok(ImmutableMap.of("tickers", data));
    }

    @Cacheable
    public Set<String> getTickers() throws SQLException {
        return new HashSet<>(StockDB.getTickers());
    }
}
