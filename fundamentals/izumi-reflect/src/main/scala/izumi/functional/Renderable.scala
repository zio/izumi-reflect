/*
 * Copyright 2019-2020 Septimal Mind Ltd
 * Copyright 2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package izumi.functional


trait Renderable[T] {
  def render(value: T): String
}

object Renderable extends WithRenderableSyntax {
  @inline def apply[T: Renderable]: Renderable[T] = implicitly
}

trait WithRenderableSyntax {

  implicit class RenderableSyntax[T: Renderable](r: T) {
    def render(): String = Renderable[T].render(r)
  }

}

