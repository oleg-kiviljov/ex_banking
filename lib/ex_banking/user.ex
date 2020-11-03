defmodule ExBanking.User do
  use GenServer

  alias __MODULE__

  @enforce_keys [:username, :wallet]
  defstruct [:username, :wallet]

  def init(username) do
    {:ok, %User{username: username, wallet: Map.new}}
  end

  def create(username) when is_binary(username) do
    case GenServer.start_link(__MODULE__, username, name: via_tuple(username)) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> {:error, :already_exists}
    end
  end

  def create(_username) do
    {:error, :wrong_arguments}
  end

  def deposit(username, amount, currency)
  when is_binary(username)
  and is_binary(currency)
  and is_number(amount)
  and amount > 0
  do
    case Registry.lookup(Registry.User, username) do
      [] -> false
      [{_pid, _}] -> GenServer.call(via_tuple(username), %{action: :deposit, amount: amount, currency: currency})
    end
  end

  def deposit(_username, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def handle_call(%{action: :deposit, amount: amount, currency: currency}, _from, state) do
    state |>
      add_currency(currency) |>
      increase_balance(amount, currency) |>
      successful_deposit(currency)
  end

  defp increase_balance(state, amount, currency) do
    %User{wallet: wallet} = state
    new_balance = Map.get(wallet, currency)  + amount
    wallet = Map.put(wallet, currency, new_balance)
    put_in(state.wallet, wallet)
  end

  def add_currency(state, currency) do
    %User{wallet: wallet} = state
    wallet = Map.put_new(wallet, currency, 0)
    put_in(state.wallet, wallet)
  end

  defp successful_deposit(state, currency) do
    %User{wallet: wallet} = state
    new_balance = Map.get(wallet, currency)
    {:reply, new_balance, state}
  end

  defp via_tuple(username), do: {:via, Registry, {Registry.User, username}}
end
