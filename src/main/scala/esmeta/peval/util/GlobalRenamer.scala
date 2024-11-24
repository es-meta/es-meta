package esmeta.peval.util

import java.util.concurrent.atomic.AtomicInteger

/* GlobalRenamer is a ESMeta runtime-level global state to distinguish forked functions */
object GlobalRenamer {
  private final var count = new AtomicInteger(0);

  def getFuncId: Int = {
    count.getAndIncrement();
  }
}
