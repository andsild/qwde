package qwde.webapi.servlets;

import com.google.common.collect.ImmutableList;
import io.micronaut.cache.annotation.CacheConfig;
import io.micronaut.cache.annotation.Cacheable;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.dataprovider.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;

import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Controller(value = "/sma", produces = MediaType.APPLICATION_JSON)
@CacheConfig("smas")
public class SimpleMovingAverage {
    private static final Logger LOG = LoggerFactory.getLogger(SimpleMovingAverage.class);
    public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
    public Map<SmaParameter, Data> smas = new HashMap<>();
    static final int WINDOW_PERIOD = 20;

    private static final class SmaParameter {
        public final String ticker;
        public final LocalDate fromDate;
        public final LocalDate toDate;

        SmaParameter(String ticker, LocalDate fromDate, LocalDate toDate) {
            this.ticker = ticker;
            this.fromDate = fromDate;
            this.toDate = toDate;
        }

        @Override
        public int hashCode() {
            return this.ticker.hashCode() + this.fromDate.hashCode() + this.toDate.hashCode();
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof SmaParameter)) {
                return false;
            }
            return this.hashCode() == o.hashCode();
        }
    }

    public static final class Data {
        public final Collection<Double> prices;
        public final Collection<Collection<Double>> sma;

        private Data(Collection<Double> prices, Collection<Collection<Double>> sma) {
            this.prices = prices;
            this.sma = sma;
        }
    }

    @Get()
    public HttpResponse<?> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate, @QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) throws SQLException {
        if (toDate.isPresent() && toDate.get().isBefore(fromDate)) {
            return HttpResponse.badRequest("toDate is before fromDate!");
        }

        LocalDate endDate = toDate.orElse(fromDate.plusDays(WINDOW_PERIOD));
        LOG.debug("Doing render with {}, {}, {}", ticker.toUpperCase(), fromDate, toDate);

        Data data = getAverages(new SmaParameter(ticker.toUpperCase(), fromDate, endDate));
        return HttpResponse.ok(data);
    }

    @Cacheable
    public Data getAverages(SmaParameter p) throws SQLException {
        CompanyStockData stockData = StockDB.getCompanyData(p.ticker, p.fromDate, p.toDate);
        if (stockData.closePrices.isEmpty()) {
            return new Data(Collections.emptyList(), Collections.emptyList());
        }

        Double[] dataAsArray = new Double[stockData.closePrices.size()];
        dataAsArray = stockData.closePrices.toArray(dataAsArray);
        List<Collection<Double>> ret = new ArrayList<>();
        for (Double[] d : MovingAverage.simpleMovingAverages(dataAsArray, 100, 10)) {
            ret.add(ImmutableList.copyOf(d));
        }

        return new Data(stockData.closePrices, ret);
    }
}
