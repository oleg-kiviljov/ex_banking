defmodule ExBanking.User do
  @moduledoc false

  use GenServer

  alias ExBanking.{Wallet}
  alias __MODULE__

  @enforce_keys [:username, :requests]
  defstruct [:username, :requests]

  @max_requests 10

  def init(username) do
    {:ok, %User{username: username, requests: 0}}
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
    process_request(username, fn() ->
      GenServer.call(via_tuple(username), %{action: :deposit, amount: amount, currency: currency})
    end)
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
    process_request(username, fn() ->
      GenServer.call(via_tuple(username), %{action: :withdraw, amount: amount, currency: currency})
    end)
  end

  def withdraw(_username, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def get_balance(username, currency)
  when is_binary(username)
  and is_binary(currency)
  do
    process_request(username, fn() ->
      GenServer.call(via_tuple(username), %{action: :get_balance, currency: currency})
    end)
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

  def send(_from_username, _to_username, _amount, _currency ) do
    {:error, :wrong_arguments}
  end

  def handle_call(%{action: :deposit, amount: amount, currency: currency}, from, state) do
    complete_request(from, fn() ->
      Wallet.create(state.username, currency)
      case Wallet.add_balance(state.username, amount, currency) do
        {:ok, new_balance} -> new_balance
        {:error, error} -> {:error, error}
      end
    end)
    track_request(state)
  end

  def handle_call(%{action: :withdraw, amount: amount, currency: currency}, from, state) do
    complete_request(from, fn() ->
      Wallet.create(state.username, currency)
      case Wallet.deduct_balance(state.username, amount, currency) do
        {:ok, new_balance} -> new_balance
        {:error, error} -> {:error, error}
      end
    end)
    track_request(state)
  end

  def handle_call(%{action: :get_balance, currency: currency}, from, state) do
    complete_request(from, fn() ->
      Wallet.create(state.username, currency)
      Wallet.get_balance(state.username, currency)
    end)
    track_request(state)
  end

  def handle_call(%{action: :get_requests}, _from, state) do
    {:reply, state.requests, state}
  end

  def handle_cast(:request_completed, state) do
    {:noreply, put_in(state.requests, state.requests - 1)}
  end

  defp complete_request(initiator, func) do
    pid = self()

    spawn_link(fn() ->
      result = func.()
      GenServer.cast(pid, :request_completed)
      GenServer.reply(initiator, result)
    end)
  end

  def process_request(username, func) do
    with {:ok, _} <- lookup_user(username),
         {:ok, _} <- allow_request(username)
    do
      func.()
    else
      {:error, error} -> {:error, error}
    end
  end

  defp track_request(state) do
    {:noreply, put_in(state.requests, state.requests + 1)}
  end

  def allow_request(username) do
    requests = GenServer.call(via_tuple(username), %{action: :get_requests})
    if requests >= @max_requests do
      {:error, :too_many_requests_to_user}
    else
      {:ok, requests}
    end
  end

  defp lookup_user(username) do
    case Registry.lookup(Registry.User, username) do
      [{pid, _}] -> {:ok, pid}
      [] -> {:error, :user_does_not_exist}
    end
  end

  defp via_tuple(username), do: {:via, Registry, {Registry.User, username}}
end
