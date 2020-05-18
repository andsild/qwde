package qwde.webapi.servlets;

import io.micronaut.cache.annotation.CacheConfig;
import io.micronaut.cache.annotation.Cacheable;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.dataprovider.db.StockDB;

import java.sql.SQLException;
import java.util.Set;
import java.util.HashSet;

@Controller(value = "/tickers", produces = MediaType.APPLICATION_JSON)
@CacheConfig("tickers")
public class Tickers {
    private static final Logger LOG = LoggerFactory.getLogger(Tickers.class);
    public Set<String> tickers = new HashSet<>();

    @Get()
    public HttpResponse<?> doGet() throws SQLException {
        Set<String> data = getTickers();
        return HttpResponse.ok(data);
    }

    @Cacheable
    public Set<String> getTickers() throws SQLException {
        return new HashSet<>(StockDB.getTickers());
    }
}
