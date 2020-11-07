defmodule ExBanking.User do
  @moduledoc false

  use GenServer

  alias ExBanking.{Wallet}
  alias __MODULE__

  @enforce_keys [:name, :requests]
  defstruct [:name, :requests]

  @max_requests 10

  def init(name) do
    {:ok, %User{name: name, requests: 0}}
  end

  def create(name) when is_binary(name) do
    case GenServer.start_link(__MODULE__, name, name: {:via, Registry, {Registry.User, name}}) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> {:error, :user_already_exists}
    end
  end

  def create(_name) do
    {:error, :wrong_arguments}
  end

  def deposit(name, amount, currency)
  when is_binary(name)
  and is_binary(currency)
  and is_number(amount)
  and amount > 0
  do
    process_request(name, currency, fn(user_pid) ->
      GenServer.call(user_pid, %{action: :deposit, amount: amount, currency: currency})
    end)
  end

  def deposit(_name, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def withdraw(name, amount, currency)
  when is_binary(name)
  and is_binary(currency)
  and is_number(amount)
  and amount > 0
  do
    process_request(name, currency, fn(user_pid) ->
      GenServer.call(user_pid, %{action: :withdraw, amount: amount, currency: currency})
    end)
  end

  def withdraw(_name, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def get_balance(name, currency)
  when is_binary(name)
  and is_binary(currency)
  do
    process_request(name, currency, fn(user_pid) ->
      GenServer.call(user_pid, %{action: :get_balance, currency: currency})
    end)
  end

  def get_balance(_name, _currency) do
    {:error, :wrong_arguments}
  end

  def send(from_name, to_name, amount, currency)
  when is_binary(from_name)
  and is_binary(to_name)
  and is_binary(currency)
  and is_number(amount)
  and amount > 0
  do
    process_request(from_name, currency, fn(sender_pid) ->
      process_request(to_name, currency, fn(beneficiary_pid) ->
        GenServer.call(sender_pid, %{action: :send, beneficiary: beneficiary_pid, amount: amount, currency: currency})
      end, :receiver_does_not_exist, :too_many_requests_to_receiver)
    end, :sender_does_not_exist, :too_many_requests_to_sender)
  end

  def send(_from_name, _to_name, _amount, _currency) do
    {:error, :wrong_arguments}
  end

  def handle_call(%{action: :deposit, amount: amount, currency: currency}, from, state) do
    complete_request(from, fn() ->
      case Wallet.add_balance(state.name, amount, currency) do
        {:ok, new_balance} -> new_balance
        {:error, error} -> {:error, error}
      end
    end)
    track_request(state)
  end

  def handle_call(%{action: :withdraw, amount: amount, currency: currency}, from, state) do
    complete_request(from, fn() ->
      case Wallet.deduct_balance(state.name, amount, currency) do
        {:ok, new_balance} -> new_balance
        {:error, error} -> {:error, error}
      end
    end)
    track_request(state)
  end

  def handle_call(%{action: :get_balance, currency: currency}, from, state) do
    complete_request(from, fn() ->
      Wallet.get_balance(state.name, currency)
    end)
    track_request(state)
  end

  def handle_call(%{action: :get_requests}, _from, state) do
    {:reply, state.requests, state}
  end

  def handle_call(%{action: :send, beneficiary: beneficiary, amount: amount, currency: currency}, from, state) do
    complete_request(from, fn() ->
      case Wallet.transfer(state.name, beneficiary, amount, currency) do
        {:ok, sender_balance, beneficiary_balance} -> {:ok, sender_balance, beneficiary_balance}
        {:error, error} -> {:error, error}
      end
    end)
    track_request(state)
  end

  def handle_cast(:request_completed, state) do
    {:noreply, put_in(state.requests, state.requests - 1)}
  end

  defp track_request(state), do: {:noreply, put_in(state.requests, state.requests + 1)}

  defp complete_request(from, func) do
    pid = self()

    spawn_link(fn() ->
      result = func.()
      GenServer.cast(pid, :request_completed)
      GenServer.reply(from, result)
    end)
  end

  def process_request(name, currency, func, lookup_error \\ :user_does_not_exist, request_error \\ :too_many_requests_to_user) do
    with :ok <- Wallet.create(name, currency),
         {:ok, pid} <- lookup_user(name, lookup_error),
         {:ok, _} <- allow_request(pid, request_error)
    do
      func.(pid)
    else
      {:error, error} -> {:error, error}
    end
  end

  defp lookup_user(name, error) do
    case Registry.lookup(Registry.User, name) do
      [{pid, _}] -> {:ok, pid}
      [] -> {:error, error}
    end
  end

  def allow_request(pid, error) do
    requests = GenServer.call(pid, %{action: :get_requests})
    if requests >= @max_requests do
      {:error, error}
    else
      {:ok, requests}
    end
  end
end
