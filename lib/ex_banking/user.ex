defmodule ExBanking.User do
  use GenServer

  alias ExBanking.{Wallet}
  alias __MODULE__

  @enforce_keys [:username]
  defstruct [:username]

  def init(username) do
    {:ok, %User{username: username}}
  end

  def create(username) when is_binary(username) do
    case GenServer.start_link(__MODULE__, username, name: via_tuple(username)) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> {:error, :user_already_exists}
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

  def send(from_username, to_username, amount, currency)
  when is_binary(from_username)
  and is_binary(to_username)
  and is_binary(currency)
  and is_number(amount)
  and amount > 0
  do
    with [{_pid, _}] <- lookup_user(from_username),
         [{_pid, _}] <- lookup_user(to_username)
    do
      Wallet.create(from_username, currency)
      Wallet.create(to_username, currency)

      case Wallet.transfer(from_username, to_username, amount, currency) do
        {:ok, from_user_balance, to_user_balance} -> {:ok, from_user_balance, to_user_balance}
        {:error, error} -> {:error, error}
      end
    else
      _ -> {:error, :user_does_not_exist}
    end
  end

  def send(_from_username, _to_username, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def handle_call(%{action: :deposit, amount: amount, currency: currency}, _from, state) do
    Wallet.create(state.username, currency)
    case Wallet.add_balance(state.username, amount, currency) do
      {:ok, new_balance} -> reply_with(state, new_balance)
      {:error, error} -> reply_with(state, {:error, error})
    end
  end

  def handle_call(%{action: :withdraw, amount: amount, currency: currency}, _from, state) do
    Wallet.create(state.username, currency)
    case Wallet.deduct_balance(state.username, amount, currency) do
      {:ok, new_balance} -> reply_with(state, new_balance)
      {:error, error} -> reply_with(state, {:error, error})
    end
  end

  def handle_call(%{action: :get_balance, currency: currency}, _from, state) do
    Wallet.create(state.username, currency)
    balance = Wallet.get_balance(state.username, currency)
    reply_with(state, balance)
  end

  defp reply_with(state, reply) do
    {:reply, reply, state}
  end

  defp lookup_user(username) do
    Registry.lookup(Registry.User, username)
  end

  defp via_tuple(username), do: {:via, Registry, {Registry.User, username}}
end
