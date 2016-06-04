/* A concept graph is a much simpler entity than an OWL ontology, but more
   flexible as a result.  This library implements a dynamic concept graph XML
   loader and permits certain kinds of inference on the graph.  (c) 2007
   F.G. McCabe
 
  Copyright (c) 2016. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */
go.concepts{
  import go.io.
  import go.xml.
  import go.dynamic.
  import go.hash.
  import go.setlib.
  import go.stdparse.
  import go.showable.

  /*
   * A Concept is a very simple animal: it has a fully qualified name, a display name and it can be serialized to XML.
   */

  Concept <~ xmlable.		  	-- Concepts have an XML representation
  Concept <~ { 
	fQname:[]=>symbol.			-- The fully qualified name
	display:[]=>string.
      }.

  /* A triple is a pair of concepts and a relationship between them. */

  Triple <~ xmlable.			-- Triples have an XML representation
  Triple <~ {
	domain:[]=>Concept.
	range:[]=>Concept.
      }.

}

