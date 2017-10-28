package ar.edu.tadp.gimnasioPokemon

import scala.annotation.tailrec

object GimnasioPokemon {

  class NoPuedeRealizarActivadadException extends RuntimeException

  trait TipoPokemon {
    def experienciaAlLevantar(peso: Int) = peso
    def levantarPesas(peso: Int, pokemon: Pokemon) = {
      if(pokemon.estado == Paralizado) {
        pokemon.cambiarEstado(KO)
      } else if (pokemon.fuerza < peso.toDouble / 10) {
        pokemon.perderEnergia(10).cambiarEstado(Paralizado)
      } else {
        pokemon.ganarExperiencia(experienciaAlLevantar(peso))
      }
    }
    
    def debilidades: Seq[TipoPokemon] = Seq.empty
    def pierdeContra(tipo: TipoPokemon) = debilidades.contains(tipo) 
  }
  case object Pelea extends TipoPokemon {
    override def experienciaAlLevantar(peso: Int) = peso * 2
  }
  case object Fuego extends TipoPokemon {
    override def debilidades = Seq(Agua)
  }
  case object Fantasma extends TipoPokemon {
    override def levantarPesas(peso: Int, p: Pokemon) =
      throw new NoPuedeRealizarActivadadException
  }
  case object Electrico extends TipoPokemon
  case object Agua extends TipoPokemon
  
  trait EstadoPokemon {
    def realizarActividad(actividad: Actividad, pokemon: Pokemon) = {
      actividad(pokemon)
    }
  }
  case object Normal extends EstadoPokemon
  case object Paralizado extends EstadoPokemon
  case object KO extends EstadoPokemon {
    override def realizarActividad(actividad: Actividad, pokemon: Pokemon) = {
      throw new NoPuedeRealizarActivadadException
    }
  }
  
  case class Dormido(turnosRestantes: Int = 3) extends EstadoPokemon {
    override def realizarActividad(actividad: Actividad, pokemon: Pokemon) = {
      if(turnosRestantes == 1) pokemon.cambiarEstado(Normal)
      else pokemon.cambiarEstado(Dormido(turnosRestantes - 1))
    }
  }

  object Especies {
    private val unosIncrementos = (10, 3, 2)
    
    implicit def tuplaAIncremento(t: (Int, Int, Int)) = {
      (p: Pokemon) => p.ganarEnergiaMaxima(t._1).ganarFuerza(t._2).ganarVelocidad(t._3)
    }
    
    val charmander = Especie(Fuego, None, 350, (10, 4, 3))
    val squirtle = Especie(Agua, None, 350, (p: Pokemon) => p.ganarFuerza(3).ganarEnergiaMaxima(10))
    val hitmonlee = Especie(Pelea, None, 350, unosIncrementos)
    val gengar = Especie(Fantasma, None, 350, unosIncrementos)
    val pikachu = Especie(Electrico, None, 350, unosIncrementos)
    val magikarp = Especie(Agua, None, 350, unosIncrementos)
  }

  type Incrementos = Pokemon => Pokemon

  case class Especie(
    tipoPrincipal: TipoPokemon,
    tipoSecundario: Option[TipoPokemon],
    resistenciaEvolutiva: Int,
    incrementos: Incrementos) {
    def tipos = Seq(tipoPrincipal) ++ tipoSecundario.toList
  }
    

  case class Pokemon(
      especie: Especie,
      estado: EstadoPokemon = Normal,
      energia: Int = 100,
      energiaMaxima: Int = 100,
      experiencia: Int = 0,
      fuerza: Int = 5,
      velocidad: Int = 5,
      nivel: Int = 1) {
    def realizarActividad(actividad: Actividad): Pokemon = {
      estado.realizarActividad(actividad, this)
    }

    def ganarExperiencia(exp: Int) = {
      val copia = copy(experiencia = experiencia + exp)
      copia.recalcularNivel
    }
    def perderEnergia(energiaAPerder: Int) = copy(energia = energia - energiaAPerder)

    def ganarVelocidad(vel: Int) = copy(velocidad = velocidad + vel)
    def ganarFuerza(fza: Int) = copy(fuerza = fuerza + fza)
    def ganarEnergiaMaxima(emax: Int) = copy(energiaMaxima = energiaMaxima + emax)
    
    def resistenciaEvolutiva = especie.resistenciaEvolutiva
    def tipoPrincipal = especie.tipoPrincipal
    
    def cambiarEstado(nuevoEstado: EstadoPokemon) = copy(estado = nuevoEstado) 

    def experienciaNecesariaParaNivel(nivel: Int): Int = {
      if (nivel == 2) {
        resistenciaEvolutiva
      } else {
        experienciaNecesariaParaNivel(nivel - 1) * 3 + resistenciaEvolutiva
      }
    }
    
    def subirNivel = {
      especie.incrementos(copy(nivel = nivel + 1)).recalcularNivel
    }

    def recalcularNivel: Pokemon = {
      if (experiencia > experienciaNecesariaParaNivel(nivel + 1)) {
        subirNivel
      } else {
        this
      }
    }
    def tipos = especie.tipos
  }

  val descansar: Actividad =
    (p: Pokemon) => {
      val newP = p.copy(energia = p.energiaMaxima)
      if(p.energia < p.energiaMaxima * 0.5 && p.estado == Normal) 
        newP.cambiarEstado(Dormido()) else newP 
    }

  def levantar(peso: Int): Actividad =
    (p: Pokemon) => p.tipoPrincipal.levantarPesas(peso, p)

  def nadar(minutos: Int): Actividad =
    (p: Pokemon) => 
      if(p.tipos.exists(_.pierdeContra(Agua))) p.cambiarEstado(KO)
      else {
        val newP = p.perderEnergia(minutos).ganarExperiencia(200 * minutos)
        if(p.tipos.contains(Agua)) newP.ganarVelocidad(minutos / 60) else newP
      }

  type Actividad = Pokemon => Pokemon
}