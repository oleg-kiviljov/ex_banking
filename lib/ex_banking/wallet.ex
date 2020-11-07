defmodule ExBanking.Wallet do
  @moduledoc false

  use GenServer

  alias __MODULE__
  alias Decimal, as: D

  @enforce_keys [:currency, :balance]
  defstruct [:currency, :balance]

  @precision 2

  def init(currency) do
    {:ok, %Wallet{currency: currency, balance: as_decimal(0)}}
  end

  def create(name, currency) do
    case GenServer.start_link(__MODULE__, currency, name: via_tuple(name, currency)) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
    end
  end

  def add_balance(name, amount, currency) do
    GenServer.call(via_tuple(name, currency), %{action: :add_balance, amount: as_decimal(amount)})
  end

  def deduct_balance(name, amount, currency) do
    GenServer.call(via_tuple(name, currency), %{action: :deduct_balance, amount: as_decimal(amount)})
  end

  def get_balance(name, currency) do
    GenServer.call(via_tuple(name, currency), %{action: :get_balance})
  end

  def transfer(from_name, to_name, amount, currency) do
    GenServer.call(via_tuple(from_name, currency), %{action: :transfer, beneficiary: to_name, amount: as_decimal(amount), currency: currency})
  end

  def handle_call(%{action: :add_balance, amount: amount}, _from, state) do
    case add_amount(state.balance, amount) do
      {:ok, new_balance} ->
        put_in(state.balance, new_balance) |>
          reply_with({:ok, new_balance})
    end
  end

  def handle_call(%{action: :deduct_balance, amount: amount}, _from, state) do
    case subtract_amount(state.balance, amount) do
      {:ok, new_balance} ->
        put_in(state.balance, new_balance) |>
          reply_with({:ok, new_balance})
      {:error, error} -> reply_with(state, {:error, error})
    end
  end

  def handle_call(%{action: :get_balance}, _from, state) do
    reply_with(state, state.balance)
  end

  def handle_call(%{action: :transfer, beneficiary: beneficiary, amount: amount, currency: currency}, _from, state) do
    with {:ok, new_balance} <- subtract_amount(state.balance, amount),
         new_beneficiary_balance <- GenServer.call(beneficiary, %{action: :deposit, amount: amount, currency: currency})
    do
      put_in(state.balance, new_balance) |>
        reply_with({:ok, new_balance, new_beneficiary_balance})
    else
      {:error, error} -> reply_with(state, {:error, error})
    end
  end

  defp add_amount(current_balance, amount) do
    {:ok, D.add(current_balance, amount)}
  end

  defp subtract_amount(current_balance, amount) do
    new_balance = D.sub(current_balance, amount)
    if D.positive?(new_balance) || D.equal?(new_balance, 0) do
      {:ok, new_balance}
    else
      {:error, :not_enough_money}
    end
  end

  defp as_decimal(amount) do
    amount |> to_string() |> D.new() |> Decimal.round(@precision)
  end

  defp reply_with(state, reply) do
    {:reply, reply, state}
  end

  defp via_tuple(name, currency), do: {:via, Registry, {Registry.Wallet, "#{name}_#{currency}"}}
end
