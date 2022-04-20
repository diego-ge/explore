// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

import java.time

// gql: import explore.model.reusability._
// gql: import explore.model.TargetGroup._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object AsterismQueriesGQL {

  @GraphQL
  trait AsterismGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query($programId: ProgramId!) {
        asterismGroup(programId: $programId) {
          nodes {
            observationIds
            asterism {
              id
            }
          }
        }

        targetGroup(programId: $programId) {
          nodes {
            observationIds
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

        observations(programId: $programId) {
          nodes {
            id
            constraintSet {
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
            }
            status
            activeStatus
            plannedTime {
              execution {
                microseconds
              }
            }
            targetEnvironment {
              asterism {
                id
              }
            }
            scienceConfiguration {
              gmosNorthLongSlit {
                filter
                disperser
                fpu
                slitWidth {
                  microarcseconds
                }
              }
              gmosSouthLongSlit {
                filter
                disperser
                fpu
                slitWidth {
                  microarcseconds
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object TargetGroup {
        type Nodes = explore.model.TargetGroup
      }

      object Observations {
        object Nodes {
          trait ConstraintSet extends model.ConstraintsSummary
          type ScienceConfiguration = model.ScienceConfiguration
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }
    }
  }

  @GraphQL
  trait UpdateTargetEnvironmentMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: BulkEditTargetEnvironmentInput!) {
        updateTargetEnvironment(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateAsterismMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: BulkEditAsterismInput!) {
        updateAsterism(input: $input) {
          id
        }
      }
    """
  }
}