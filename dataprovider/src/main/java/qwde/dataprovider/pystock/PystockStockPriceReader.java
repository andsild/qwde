package qwde.dataprovider.pystock;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.dataprovider.models.StockPrice;
import qwde.dataprovider.util.FileUtil;
import qwde.dataprovider.util.StockPriceReader;

public class PystockStockPriceReader implements StockPriceReader {
  private static final Logger LOG = LoggerFactory.getLogger(PystockStockPriceReader.class);

  private final List<StockPrice> stockPrices;
  private static final String DATA_FOLDER = "pystock-data";
  public static final DateTimeFormatter DATETIMEFORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

  private static boolean fileNameMatchesDate(String fileName, Set<LocalDate> dates) {
    try {
      LocalDate fileNameAsDate = LocalDate.parse(fileName, DATETIMEFORMATTER);
      return dates.contains(fileNameAsDate);
    } catch (DateTimeParseException exception) {
      return false;
    }
  }

  private static Set<LocalDate> getDateRange(LocalDate startDate, LocalDate endDate) {
    return startDate.datesUntil(endDate).filter(d -> !(d.getDayOfWeek() == DayOfWeek.SATURDAY || d.getDayOfWeek() == DayOfWeek.SUNDAY)).collect(Collectors.toSet());
  }

  @SuppressWarnings("PMD.ExceptionAsFlowControl")
  public PystockStockPriceReader(Predicate<String> fileNameFilter) throws IOException {
      // Not pretty, but it covers most use-cases by other devs.
      // I.e., we search in XDG_DATA_HOME, XDG_DATA_DIRS, and the current working directory, and the directory above
    Optional<Path> pystockDataPath = Stream.of(FileUtil.findFolderInDatapath(DATA_FOLDER), FileUtil.findInPath(DATA_FOLDER, "."), FileUtil.findInPath(DATA_FOLDER, ".."), FileUtil.findInPath(DATA_FOLDER, "../dataprovider")).filter(Optional::isPresent).map(Optional::get).findFirst();
    if (pystockDataPath.isEmpty()) {
      throw new FileNotFoundException("Unable to find pystock-data files. See README.md for more documentation.");
    }
    LOG.debug("Located pystock-data in {}", pystockDataPath.get());
    try {
      LOG.info("Reading pystock-data...");
      this.stockPrices = Files.walk(pystockDataPath.get())
        .map(Path::toFile)
        .filter(File::isFile)
        .filter(file -> file.getName().endsWith(".tar.gz"))
        .filter(f -> fileNameFilter.test(FilenameUtils.getBaseName(FilenameUtils.getBaseName(f.getName()))))
        .map(file -> {
          try {
            LOG.trace("Parsing {}", file);
            return Files.newInputStream(file.toPath());
          } catch (IOException exception) {
            throw new UncheckedIOException(exception);
          }
        })
        .flatMap(gz -> {
          try {
            return getPricesFromCompressedArchive(gz).stream();
          } catch (IOException e) {
            throw new UncheckedIOException(e);
          }
        })
      .sorted().distinct()
      .collect(Collectors.toList());

      LOG.info("{} entries loaded in memory from pystock-data", this.stockPrices.size());
    } catch (UncheckedIOException exception) {
      throw new IOException(exception);
    }
  }

  public static PystockStockPriceReader getPystockStockPriceReader(LocalDate startDate, LocalDate endDate) throws IOException {
    Set<LocalDate> desirableDates = getDateRange(startDate, endDate.plusDays(1));
    return new PystockStockPriceReader(f -> fileNameMatchesDate(f, desirableDates));
  }

  public static PystockStockPriceReader fromDate(LocalDate date) throws IOException {
    return getPystockStockPriceReader(date, date);
  }

  @SuppressWarnings("PMD.AssignmentInOperand")
  private static List<StockPrice> getPricesFromCompressedArchive(InputStream compressedArchive) throws IOException {
    TarArchiveInputStream stream = new TarArchiveInputStream(new GzipCompressorInputStream(compressedArchive));
    TarArchiveEntry entry;
    while ((entry = stream.getNextTarEntry()) != null) {
      if ("prices.csv".equals(entry.getName())) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))) {
          // Skip header
          return reader.lines().skip(1).map(PystockStockPriceReader::parseStockPrice).filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList());
        }
      }
    }

    return Collections.emptyList();
  }

  private static Optional<StockPrice> parseStockPrice(String line) {
    if (line.startsWith("symbol")) {
      return Optional.empty();
    }
    String[] tokens = line.split(",");
    return Optional.of(new PystockStockPrice(new BigDecimal(tokens[3]), new BigDecimal(tokens[4]), new BigDecimal(tokens[5]), Long.parseLong(tokens[6]), tokens[0], parseTimestamp(tokens[1])));
  }

  private static LocalDateTime parseTimestamp(String line) {
    String[] tokens = line.split("-");
    return LocalDateTime.of(Integer.parseInt(tokens[0]), Integer.parseInt(tokens[1]), Integer.parseInt(tokens[2]), 0, 0, 0, 0);
  }

  @Override
  public List<StockPrice> read() {
    return this.stockPrices;
  }
}
