// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

// gql: import explore.model.reusability._
// gql: import explore.model.TargetWithId._
// gql: import lucuma.ui.reusability._

object TargetQueriesGQL {

  @GraphQL
  trait TargetNameQuery extends GraphQLOperation[ObservationDB] {
    // FIXME Change this to an actual name pattern query when it's available in the API
    val document = """
      query($programId: ProgramId!) {
        targetGroup(programId: $programId) {
          nodes {
            target {
              id
              name
              sidereal {
                ra {
                  microarcseconds
                }
                dec {
                  microarcseconds
                }
                epoch
                properMotion {
                  ra {
                    microarcsecondsPerYear
                  }
                  dec {
                    microarcsecondsPerYear
                  }
                }
                radialVelocity {
                  centimetersPerSecond
                }
                parallax {
                  microarcseconds
                }
                catalogInfo {
                  name
                  id
                  objectType
                }
              }
              sourceProfile {
                point {
                  bandNormalized {
                    sed {
                      stellarLibrary
                      coolStar
                      galaxy
                      planet
                      quasar
                      hiiRegion
                      planetaryNebula
                      powerLaw
                      blackBodyTempK
                      fluxDensities {
                        wavelength {
                          picometers
                        }
                        density
                      }
                    }
                    brightnesses {
                      band
                      value
                      units
                      error
                    }
                  }
                  emissionLines {
                    lines {
                      wavelength {
                        picometers
                      }
                      lineWidth
                      lineFlux {
                        value
                        units
                      }
                    }
                    fluxDensityContinuum {
                      value
                      units
                    }
                  }
                }
                uniform {
                  bandNormalized {
                    sed {
                      stellarLibrary
                      coolStar
                      galaxy
                      planet
                      quasar
                      hiiRegion
                      planetaryNebula
                      powerLaw
                      blackBodyTempK
                      fluxDensities {
                        wavelength {
                          picometers
                        }
                        density
                      }
                    }
                    brightnesses {
                      band
                      value
                      units
                      error
                    }
                  }
                  emissionLines {
                    lines {
                      wavelength {
                        picometers
                      }
                      lineWidth
                      lineFlux {
                        value
                        units
                      }
                    }
                    fluxDensityContinuum {
                      value
                      units
                    }
                  }
                }
                gaussian {
                  fwhm {
                    microarcseconds
                  }
                  bandNormalized {
                    sed {
                      stellarLibrary
                      coolStar
                      galaxy
                      planet
                      quasar
                      hiiRegion
                      planetaryNebula
                      powerLaw
                      blackBodyTempK
                      fluxDensities {
                        wavelength {
                          picometers
                        }
                        density
                      }
                    }
                    brightnesses {
                      band
                      value
                      units
                      error
                    }
                  }
                  emissionLines {
                    lines {
                      wavelength {
                        picometers
                      }
                      lineWidth
                      lineFlux {
                        value
                        units
                      }
                    }
                    fluxDensityContinuum {
                      value
                      units
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object TargetGroup {
        object Nodes {
          type Target = model.TargetWithId
        }
      }
    }
  }

  @GraphQL
  trait CreateTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($programId: ProgramId! $input: CreateTargetInput!) {
        createTarget(programId: $programId, input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait DeleteTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        deleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UndeleteTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        undeleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditTargetInput!) {
        updateTarget(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetMutationWithResult extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditTargetInput!) {
        updateTarget(input: $input) {
          id
          name
          sidereal {
            ra {
              microarcseconds
            }
            dec {
              microarcseconds
            }
            epoch
            properMotion {
              ra {
                microarcsecondsPerYear
              }
              dec {
                microarcsecondsPerYear
              }
            }
            radialVelocity {
              centimetersPerSecond
            }
            parallax {
              microarcseconds
            }
            catalogInfo {
              name
              id
              objectType
            }
          }
          sourceProfile {
            point {
              bandNormalized {
                sed {
                  stellarLibrary
                  coolStar
                  galaxy
                  planet
                  quasar
                  hiiRegion
                  planetaryNebula
                  powerLaw
                  blackBodyTempK
                  fluxDensities {
                    wavelength {
                      picometers
                    }
                    density
                  }
                }
                brightnesses {
                  band
                  value
                  units
                  error
                }
              }
              emissionLines {
                lines {
                  wavelength {
                    picometers
                  }
                  lineWidth
                  lineFlux {
                    value
                    units
                  }
                }
                fluxDensityContinuum {
                  value
                  units
                }
              }
            }
            uniform {
              bandNormalized {
                sed {
                  stellarLibrary
                  coolStar
                  galaxy
                  planet
                  quasar
                  hiiRegion
                  planetaryNebula
                  powerLaw
                  blackBodyTempK
                  fluxDensities {
                    wavelength {
                      picometers
                    }
                    density
                  }
                }
                brightnesses {
                  band
                  value
                  units
                  error
                }
              }
              emissionLines {
                lines {
                  wavelength {
                    picometers
                  }
                  lineWidth
                  lineFlux {
                    value
                    units
                  }
                }
                fluxDensityContinuum {
                  value
                  units
                }
              }
            }
            gaussian {
              fwhm {
                microarcseconds
              }
              bandNormalized {
                sed {
                  stellarLibrary
                  coolStar
                  galaxy
                  planet
                  quasar
                  hiiRegion
                  planetaryNebula
                  powerLaw
                  blackBodyTempK
                  fluxDensities {
                    wavelength {
                      picometers
                    }
                    density
                  }
                }
                brightnesses {
                  band
                  value
                  units
                  error
                }
              }
              emissionLines {
                lines {
                  wavelength {
                    picometers
                  }
                  lineWidth
                  lineFlux {
                    value
                    units
                  }
                }
                fluxDensityContinuum {
                  value
                  units
                }
              }
            }
          }
        }
      }
    """

    object Data {
      type UpdateTarget = model.TargetWithId
    }
  }

  @GraphQL
  trait CloneTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($existingTargetId: TargetId!, $observationIds: [ObservationId!]) {
        cloneTarget(existingTargetId: $existingTargetId, observationIds: $observationIds) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramTargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($programId: ProgramId!) {
        targetEdit(programId: $programId) {
          id
        }
      }
    """
  }
}