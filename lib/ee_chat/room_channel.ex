defmodule EEChat.RoomChannel do
  use Phoenix.Channel

  def join("room:lobby", _payload, socket) do
    {:ok, socket}
  end

  def handle_in("new:message", payload, socket) do
    broadcast! socket, "new:message", payload
    {:noreply, socket}
  end

end
