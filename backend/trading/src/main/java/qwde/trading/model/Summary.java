package qwde.backend.trading.model;

import com.google.common.collect.ImmutableList;

import java.util.List;

public final class Summary {
    public final List<Order> buyOrders;
    public final List<Order> sellOrders;
    public final List<MarketOrder> buyHistory;
    public final List<Trade> trades;
    public final List<Order> pendingBuyOrders;
    public final List<Order> pendingAskOrders;

    public Summary(List<Order> buyOrders, List<Order> sellOrders, List<MarketOrder> buyHistory, List<Trade> trades, List<Order> pendingBuyOrders, List<Order> pendingAskOrders) {
        this.buyOrders = buyOrders == null ? ImmutableList.of() : ImmutableList.copyOf(buyOrders);
        this.sellOrders = sellOrders == null ? ImmutableList.of() : ImmutableList.copyOf(sellOrders);
        this.buyHistory = buyHistory == null ? ImmutableList.of() : ImmutableList.copyOf(buyHistory);
        this.trades = trades == null ? ImmutableList.of() : ImmutableList.copyOf(trades);
        this.pendingBuyOrders = pendingBuyOrders == null ? ImmutableList.of() : ImmutableList.copyOf(pendingBuyOrders);
        this.pendingAskOrders = pendingAskOrders == null ? ImmutableList.of() : ImmutableList.copyOf(pendingAskOrders);
    }
}

