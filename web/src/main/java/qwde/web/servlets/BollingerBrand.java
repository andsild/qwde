package qwde.web.servlets;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Streams;
import io.micronaut.core.convert.format.Format;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.reactivex.Single;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.analytics.aggregate.MovingAverage;
import qwde.analytics.aggregate.StandardDeviation;
import qwde.analytics.db.StockDB;
import qwde.dataprovider.models.CompanyStockData;
import qwde.web.plotly.FigureTemplate;
import qwde.web.plotly.LinePlotRenderer;
import qwde.web.plotly.PageRenderer;
import tech.tablesaw.plotly.traces.ScatterTrace;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Controller(value = "/bb", produces = MediaType.TEXT_HTML)
public final class BollingerBrand {
  private static final Logger LOG = LoggerFactory.getLogger(BollingerBrand.class);
  public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");
  private static final int SMOOTHING_PERIOD = 20;

  @Get("/{ticker}/{fromDate}")
  public Single<String> doGet(String ticker, @Format("yyyyMMdd") LocalDate fromDate, @QueryValue @Format("yyyyMMdd") Optional<LocalDate> toDate) throws SQLException {
    if (toDate.isPresent() && toDate.get().isBefore(fromDate)) {
      return Single.just("toDate is before fromDate!");
    }

    LocalDate endDate = toDate.orElse(fromDate.plusDays(SMOOTHING_PERIOD));
    LOG.debug("Doing render with {}, {}, {}", ticker, fromDate, toDate);

    CompanyStockData stockData;
    stockData = StockDB.getCompanyData(ticker.toUpperCase(), fromDate.minusDays(SMOOTHING_PERIOD), endDate);

    if (stockData.closePrices.isEmpty()) {
      return Single.just("No data found. Are you sure the ticker and date were correct?");
    }

    List<ScatterTrace> traces = new ArrayList<>();
    traces.add(LinePlotRenderer.genScatterPlot(stockData.closePrices.subList(SMOOTHING_PERIOD, stockData.closePrices.size()), "price"));
    traces.addAll(getBollingerBrand(stockData, SMOOTHING_PERIOD));

    String text = "<div style=\"text-align:center\">Formula taken from <a href=\"https://www.investopedia.com/terms/b/bollingerbands.asp\">investopedia</a></div>"
                    + "$$\n\\left\\{\n\\begin{aligned}\nBOLU&=MA(TP,n)+m*\\sigma{}\\\\\nBOLD&=MA(TP,n)-m*\\sigma{}\n\\end{aligned}\n\\right.\n\\\\\n\\\\\\\n\\textbf{where}\\\\\n"
                    + "\\begin{aligned}\nBOLU &= \\text{Upper Bollinger Band}\\\\\nBOLD&=\\text{Lower Bollinger Band}\\\\\nMA&=\\text{Moving average}\\\\\nTP (\\text{typical price)} &= \\frac{High + Low + Close}{3}\\\\\n"
                    + "\\text{Number of days in smoothing period} &=  20\\\\\nm&=\\text{Number of standard deviations} = 2\\\\\n\\sigma{}&=\\text{Standard Deviation over last n periods of TP}\n\\end{aligned}\n$$";

    return Single.just(PageRenderer.renderPage("graphpage.ftl", ImmutableMap.of("pageTitle", "Bollinger Band", "figures", Collections.singletonList(
            new FigureTemplate(LinePlotRenderer.scatterPlot(traces, ScatterTrace.class, ticker, "day", "closing price"), "Stock closing prices and BollingerBands, windows size = 20", text))
    )));
  }

  private static List<ScatterTrace> getBollingerBrand(CompanyStockData stockData, int windowSize) {
    List<Double> data = new ArrayList<>();
    for (int i = 0; i < stockData.closePrices.size(); i++) {
      data.add((stockData.closePrices.get(i) + stockData.highPrices.get(i) + stockData.lowPrices.get(i)) / 3.0);
    }

    Double[] dataAsArray = new Double[data.size()];
    dataAsArray = data.toArray(dataAsArray);
    List<Double> mean = Arrays.asList(MovingAverage.simpleMovingAverage(dataAsArray, windowSize)).subList(SMOOTHING_PERIOD, data.size());
    List<Double> stdDev = StandardDeviation.rollingStandardDeviation(data, windowSize)
            .stream()
            .skip(windowSize)
            .map(x -> x * 2)
            .collect(Collectors.toList());
    List<Double> standardDeviationUpper = Streams.zip(mean.stream(), stdDev.stream(), Double::sum).collect(Collectors.toList());
    List<Double> standardDeviationLower = Streams.zip(mean.stream(), stdDev.stream(), (m, s) -> m - s).collect(Collectors.toList());

    return Arrays.asList(
        LinePlotRenderer.genScatterPlot(standardDeviationUpper, "upper BB"),
        LinePlotRenderer.genScatterPlot(mean, "mean"),
        LinePlotRenderer.genScatterPlot(standardDeviationLower, "lower BB")
    );
  }
}
