package ar.edu.tadp.gimnasioPokemon

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ar.edu.tadp.gimnasioPokemon.GimnasioPokemon._

class GimnasioSpec extends FlatSpec with Matchers {
  "Un pokemon" should "realizar actividad descansar y recuperar su energia" in {
    val pokemon = Pokemon(Especies.charmander, energia = 20, energiaMaxima = 100)

    //    pokemon.descansar
    //    descansar(pokemon)
    //    realizarActividad(descansar, pokemon)

    val pokemonDescansado = pokemon.realizarActividad(descansar)

    pokemonDescansado.energia should be(pokemon.energiaMaxima)
  }

  "Un charmander" should "realizar actividad levantar pesas y ganar experiencia" in {
    val charmander = Pokemon(Especies.charmander)

    val charmanderFortalecido =
      charmander.realizarActividad(levantar(1))

    charmanderFortalecido.experiencia should be(charmander.experiencia + 1)
  }

  "Un hitmonlee" should "realizar actividad levantar pesas y ganar experiencia por 2" in {
    val hitmonlee = Pokemon(Especies.hitmonlee)

    val hitmonleeFortalecido =
      hitmonlee.realizarActividad(levantar(1))

    hitmonleeFortalecido.experiencia should be(hitmonlee.experiencia + 2)
  }

  "Un gengar" should "no poder levantar pesas" in {
    val gengar = Pokemon(Especies.gengar)

    a[NoPuedeRealizarActivadadException] should be thrownBy {
      gengar.realizarActividad(levantar(1))
    }
  }

  "Un charmander" should "realizar actividad levantar pesas con mucho peso y perder energia y no ganar experiencia" in {
    val charmander = Pokemon(Especies.charmander, fuerza = 5)

    val charmanderDebilitado =
      charmander.realizarActividad(levantar(51))

    charmanderDebilitado.experiencia should be(charmander.experiencia)
    charmanderDebilitado.energia should be(charmander.energia - 10)
    charmanderDebilitado.estado should be(Paralizado)
  }

  "Un pikachu" should "realizar actividad nadar y ganar experiencia y perder energia" in {
    val pikachu = Pokemon(Especies.pikachu)

    val pikachuNadador =
      pikachu.realizarActividad(nadar(2))

    pikachuNadador.experiencia should be(pikachu.experiencia + 400)
    pikachuNadador.energia should be(pikachu.energia - 2)
  }

  "Un squirtle" should "realizar actividad nadar y ganar experiencia y perder energia y ganar velocidad" in {
    val squirtle = Pokemon(Especies.squirtle)

    val squirtleNadador =
      squirtle.realizarActividad(nadar(60))

    squirtleNadador.experiencia should be(squirtle.experiencia + 60 * 200)
    squirtleNadador.energia should be(squirtle.energia - 60)
    squirtleNadador.velocidad should be(squirtle.velocidad + 1)
  }

  "Un magikarp" should "realizar actividad nadar y ganar experiencia y subir de nivel" in {
    val magikarp = Pokemon(Especies.magikarp)

    magikarp.nivel should be(1)

    val magikarpNadador =
      magikarp.realizarActividad(nadar(8))

    magikarpNadador.nivel should be(3)
    (magikarpNadador.energiaMaxima,
      magikarpNadador.fuerza,
      magikarpNadador.velocidad) should be(120, 11, 9)
  }

  "Un pokemon paralizado" should "levantar pesas y quedar KO" in {
    val pikachu = Pokemon(Especies.pikachu, estado = Paralizado)

    val pikachuKO = pikachu.realizarActividad(levantar(1))

    pikachuKO.estado should be(KO)
  }

  "Un pokemon KO" should "no debe poder realizar ninguna actividad" in {
    val pikachu = Pokemon(Especies.pikachu, estado = KO)

    a[NoPuedeRealizarActivadadException] should be thrownBy {
      val pikachuKO = pikachu.realizarActividad(levantar(1))
    }
  }
  
  "Un charmander" should "quedar KO cuando nada" in {
    val charmander = Pokemon(Especies.charmander)

    val charmanderKo = charmander.realizarActividad(nadar(1))

    charmanderKo.estado should be(KO)
  }
  
  "Un charmander" should "quedar dormido si tiene menos del 50% de la energía al descansar" in {
    val charmander = Pokemon(Especies.charmander, energia = 20)

    val charmanderDormido = charmander.realizarActividad(descansar)

    charmanderDormido.estado should be(Dormido(3))
   
    var char = charmanderDormido
    (1 to 2).foreach { i =>
      char = char.realizarActividad(levantar(1))
      char should be (charmanderDormido
          .copy(estado = Dormido(3 - i)))      
    }
    
    char = char.realizarActividad(levantar(1))
    char.estado should be (Normal)
  }

  //  it should "throw NoSuchElementException if an empty stack is popped" in {
  //    val emptyStack = new Stack[Int]
  //    a [NoSuchElementException] should be thrownBy {
  //      emptyStack.pop()
  //    } 
  //  }
}