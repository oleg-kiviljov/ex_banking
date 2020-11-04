defmodule ExBanking.Wallet do
  use GenServer

  alias __MODULE__
  alias Decimal, as: D

  @enforce_keys [:currency, :balance]
  defstruct [:currency, :balance]

  def init(currency) do
    {:ok, %Wallet{currency: currency, balance: D.new(0)}}
  end

  def create(username, currency) do
    case GenServer.start_link(__MODULE__, currency, name: via_tuple(username, currency)) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> {:error, :already_exists}
    end
  end

  def add_balance(username, amount, currency) do
    GenServer.call(via_tuple(username, currency), %{action: :add_balance, amount: amount})
  end

  def deduct_balance(username, amount, currency) do
    GenServer.call(via_tuple(username, currency), %{action: :deduct_balance, amount: amount})
  end

  def get_balance(username, currency) do
    GenServer.call(via_tuple(username, currency), %{action: :get_balance})
  end

  def handle_call(%{action: :add_balance, amount: amount}, _from, state) do
    new_balance = D.add(state.balance, D.new(amount))
    put_in(state.balance, new_balance) |>
      reply_with(new_balance)
  end

  def handle_call(%{action: :deduct_balance, amount: amount}, _from, state) do
    new_balance = D.sub(state.balance, D.new(amount))
    cond do
      D.compare(new_balance, 0) == :gt || D.equal?(new_balance, 0) ->
        put_in(state.balance, new_balance) |>
        reply_with({:ok, new_balance})
      true ->
        reply_with(state, {:error, :not_enough_money})
    end
  end

  def handle_call(%{action: :get_balance}, _from, state) do
    reply_with(state, state.balance)
  end

  defp reply_with(state, reply) do
    {:reply, reply, state}
  end

  defp via_tuple(username, currency), do: {:via, Registry, {Registry.Wallet, "#{username}_#{currency}"}}
end
