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
      {:error, {:already_started, _pid}} -> {:error, :wallet_already_exists}
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

  def transfer(from_username, to_username, amount, currency) do
    GenServer.call(via_tuple(from_username, currency), %{action: :transfer, beneficiary: to_username, amount: amount, currency: currency})
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
         {:ok, new_beneficiary_balance} <- GenServer.call(via_tuple(beneficiary, currency), %{action: :add_balance, amount: amount, currency: currency})
    do
      put_in(state.balance, new_balance) |>
        reply_with({:ok, new_balance, new_beneficiary_balance})
    else
      {:error, error} -> reply_with(state, {:error, error})
    end
  end

  defp add_amount(current_balance, amount) do
    {:ok, D.add(current_balance, D.new(amount))}
  end

  defp subtract_amount(current_balance, amount) do
    new_balance = D.sub(current_balance, D.new(amount))
    if D.compare(new_balance, 0) == :gt || D.equal?(new_balance, 0) do
      {:ok, new_balance}
    else
      {:error, :not_enough_money}
    end
  end

  defp reply_with(state, reply) do
    {:reply, reply, state}
  end

  defp via_tuple(username, currency), do: {:via, Registry, {Registry.Wallet, "#{username}_#{currency}"}}
end
