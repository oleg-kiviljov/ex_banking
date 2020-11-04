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
    case lookup_user(username) do
      [] -> {:error, :user_does_not_exist}
      [{_pid, _}] -> GenServer.call(via_tuple(username), %{action: :deposit, amount: amount, currency: currency})
    end
  end

  def deposit(_username, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def withdraw(username, amount, currency)
  when is_binary(username)
  and is_binary(currency)
  and is_number(amount)
  and amount > 0
  do
    case lookup_user(username) do
      [] -> {:error, :user_does_not_exist}
      [{_pid, _}] -> GenServer.call(via_tuple(username), %{action: :withdraw, amount: amount, currency: currency})
    end
  end

  def withdraw(_username, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def get_balance(username, currency)
  when is_binary(username)
  and is_binary(currency)
  do
    case lookup_user(username) do
      [] -> {:error, :user_does_not_exist}
      [{_pid, _}] -> GenServer.call(via_tuple(username), %{action: :get_balance, currency: currency})
    end
  end

  def get_balance(_username, _currency) do
    {:error, :wrong_arguments}
  end

  def handle_call(%{action: :deposit, amount: amount, currency: currency}, _from, state) do
    with {:ok, new_balance} <- confirm_deposit(state, amount, currency)
    do
      state |>
      add_currency(currency) |>
      increase_balance(amount, currency) |>
      reply_with(new_balance)
    end
  end

  def handle_call(%{action: :withdraw, amount: amount, currency: currency}, _from, state) do
    with {:ok, new_balance} <- confirm_withdraw(state, amount, currency)
    do
      state |>
      decrease_balance(amount, currency) |>
      reply_with(new_balance)
    else
      _ -> reply_with(state, {:error, :not_enough_money})
    end
  end

  def handle_call(%{action: :get_balance, currency: currency}, _from, state) do
    balance = retrieve_balance(state, currency)
    reply_with(state, balance)
  end

  defp confirm_deposit(%User{wallet: wallet}, amount, currency) do
    new_balance = case Map.get(wallet, currency) do
      nil -> 0 + amount
      value -> value + amount
    end
    if new_balance >= 0, do: {:ok, new_balance}
  end

  defp confirm_withdraw(%User{wallet: wallet}, amount, currency) do
    new_balance = case Map.get(wallet, currency) do
      nil -> 0 - amount
      value -> value - amount
    end
    if new_balance >= 0, do: {:ok, new_balance}
  end

  defp add_currency(state, currency) do
    %User{wallet: wallet} = state
    wallet = Map.put_new(wallet, currency, 0)
    put_in(state.wallet, wallet)
  end

  defp increase_balance(state, amount, currency) do
    %User{wallet: wallet} = state
    new_balance = Map.get(wallet, currency) + amount
    wallet = Map.put(wallet, currency, new_balance)
    put_in(state.wallet, wallet)
  end

  defp decrease_balance(state, amount, currency) do
    %User{wallet: wallet} = state
    new_balance = Map.get(wallet, currency) - amount
    wallet = Map.put(wallet, currency, new_balance)
    put_in(state.wallet, wallet)
  end

  def retrieve_balance(state, currency) do
    %User{wallet: wallet} = state
    case Map.get(wallet, currency) do
      nil -> 0
      value -> value
    end
  end

  defp reply_with(state, reply) do
    {:reply, reply, state}
  end

  defp lookup_user(username) do
    Registry.lookup(Registry.User, username)
  end

  defp via_tuple(username), do: {:via, Registry, {Registry.User, username}}
end
