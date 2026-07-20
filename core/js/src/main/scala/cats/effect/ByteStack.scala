/*
 * Copyright 2020-2025 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.effect

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind, ModuleKind}
import scala.scalajs.js

trait ByteStackSchema {
  type T

  val nil: T

  @inline def access(lst: T, index: Int): Int
  @inline def decrement(lst: T, index: Int): Unit
  @inline def increment(lst: T, index: Int): Unit
  @inline def set(lst: T, index: Int, value: Int): Unit

  @inline def create(initialMaxOps: Int): T

  @inline def growIfNeeded(stack: T, count: Int): T

  @inline def push(stack: T, op: Byte): T = {
    val c = access(stack, 0) // current count of elements
    val use = growIfNeeded(stack, c) // alias so we add to the right place
    val s = (c >> 3) + 1 // current slot in `use`
    val shift = (c & 7) << 2 // BEGIN MAGIC
    set(use, s, (access(use, s) & ~(0xffffffff << shift)) | (op << shift)) // END MAGIC
    increment(use, 0) // write the new count
    use
  }

  @inline final def size(stack: T): Int =
    access(stack, 0)

  @inline final def isEmpty(stack: T): Boolean =
    access(stack, 0) < 1

  @inline final def read(stack: T, pos: Int): Byte = {
    if (pos < 0 || pos >= access(stack, 0)) throw new ArrayIndexOutOfBoundsException()
    ((access(stack, (pos >> 3) + 1) >>> ((pos & 7) << 2)) & 0x0000000f).toByte
  }

  @inline final def peek(stack: T): Byte = {
    val c = access(stack, 0) - 1
    if (c < 0) throw new ArrayIndexOutOfBoundsException()
    ((access(stack, (c >> 3) + 1) >>> ((c & 7) << 2)) & 0x0000000f).toByte
  }

  @inline final def pop(stack: T): Byte = {
    val op = peek(stack)
    decrement(stack, 0)
    op
  }
}

private object JSByteStack extends ByteStackSchema {
  type T = js.Array[Int]
  
  val nil = null

  @inline override def access(lst: T, index: Int): Int = lst(index)
  @inline override def decrement(lst: T, index: Int): Unit = lst(index) -= 1
  @inline override def increment(lst: T, index: Int): Unit = lst(index) += 1
  @inline override def set(lst: T, index: Int, value: Int): Unit = lst(index) = value

  @inline override def create(initialMaxOps: Int): T = {
    val _ = initialMaxOps
    js.Array(0)
  }

  @inline override def growIfNeeded(stack: T, count: Int): T = {
    if ((1 + ((count + 1) >> 3)) < stack.length) {
      stack
    } else {
      stack.appended(0)
    }
  }
}


private object WasiByteStack extends ByteStackSchema {

  type T = Array[Int]

  val nil = null

  @inline def access(lst: T, index: Int): Int = lst(index)
  @inline def decrement(lst: T, index: Int): Unit = lst(index) -= 1
  @inline def increment(lst: T, index: Int): Unit = lst(index) += 1
  @inline def set(lst: T, index: Int, value: Int): Unit = lst(index) = value

  @inline final def create(initialMaxOps: Int): T = {
    val _ = initialMaxOps
    Array(0)
  }

  @inline final def growIfNeeded(stack: T, count: Int): T = {
    if ((1 + ((count + 1) >> 3)) < stack.length) {
      stack
    } else {
      val bigger = new Array[Int](stack.length << 1)
      System.arraycopy(stack, 0, bigger, 0, stack.length) // Count in stack(0) copied "for free"
      bigger
    }
  }
}

private object ByteStack {
  val ops =
    linkTimeIf(moduleKind == ModuleKind.WasmComponent) {
      WasiByteStack : ByteStackSchema
    } {
      JSByteStack
    }

  type T = ops.T

  val nil: T = ops.nil

  @inline final def create(initialMaxOps: Int): T = ops.create(initialMaxOps)

  @inline final def growIfNeeded(stack: T, count: Int): T = ops.growIfNeeded(stack, count)

  @inline final def push(stack: T, op: Byte): T = ops.push(stack, op)

  @inline final def size(stack: T): Int = ops.size(stack)

  @inline final def isEmpty(stack: T): Boolean = ops.isEmpty(stack)

  @inline final def read(stack: T, pos: Int): Byte = ops.read(stack, pos)

  @inline final def peek(stack: T): Byte = ops.peek(stack)

  @inline final def pop(stack: T): Byte = ops.pop(stack)
}
