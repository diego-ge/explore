// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import lucuma.core.{model => coreModel}
import lucuma.schemas.ObservationDB

import java.time
// gql: import explore.model.reusability._
// gql: import explore.model.TargetWithId._
// gql: import io.circe.refined._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object ObsQueriesGQL {
  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    // TODO We should do a single observations query and extract the constraint sets and targets from it.
    val document = """
      query($programId: ProgramId!) {
        observations(programId: $programId) {
          nodes {
            id
            title
            subtitle
            constraintSet {
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
            }
            status
            activeStatus
            visualizationTime
            plannedTime {
              execution {
                microseconds
              }
            }
            scienceMode {
              gmosNorthLongSlit {
                basic {
                  grating
                  filter
                  fpu
                }
                advanced {
                  overrideGrating
                  overrideFilter
                  overrideFpu
                  explicitXBin
                  explicitYBin
                  explicitAmpReadMode
                  explicitAmpGain
                  explicitRoi
                  explicitWavelengthDithers
                  explicitSpatialOffsets {
                    microarcseconds
                  }
                }
              }
              gmosSouthLongSlit {
                basic {
                  grating
                  filter
                  fpu
                }
                advanced {
                  overrideGrating
                  overrideFilter
                  overrideFpu
                  explicitXBin
                  explicitYBin
                  explicitAmpReadMode
                  explicitAmpGain
                  explicitRoi
                  explicitWavelengthDithers
                  explicitSpatialOffsets {
                    microarcseconds
                  }
                }
              }
            }
          }
        }

        constraintSetGroup(programId: $programId) {
          nodes {
            constraintSet {
              cloudExtinction
              imageQuality
              skyBackground
              waterVapor
              elevationRange {
                airMass {
                  min
                  max
                }
                hourAngle {
                  minHours
                  maxHours
                }
              }
            }
            observations {
              nodes {
                id
              }
            }
          }
        }

        targetGroup(programId: $programId) {
          nodes {
            observationIds
            target {
              id
              sidereal {
                ra {
                  microarcseconds
                }
                dec {
                  microarcseconds
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object Observations {
        object Nodes {
          trait ConstraintSet extends ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
          type ScienceMode = model.ScienceMode
        }
      }

      object ConstraintSetGroup {
        type Nodes = model.ConstraintGroup
      }

      object TargetGroup {
        object Nodes {
          object Target {
            type Sidereal = lucuma.core.math.Coordinates
          }
        }
      }
    }

  }

  @GraphQL
  trait ProgramObservationsEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($programId: ProgramId!) {
        observationEdit(programId: $programId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($createObservation: CreateObservationInput!) {
        createObservation(input: $createObservation) {
          id
          title
          subtitle
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
        }
      }
    """

    object Data {
      object CreateObservation {
        object TargetEnvironment {
          object Asterism {
            type Sidereal = lucuma.core.math.Coordinates
          }
        }
        trait ConstraintSet extends ConstraintsSummary
        object PlannedTime       {
          type Execution = time.Duration
        }
      }
    }
  }

  @GraphQL
  trait ProgramDeleteObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: DeleteObservationInput!) {
        deleteObservation(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramUndeleteObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UndeleteObservationInput!) {
        undeleteObservation(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ObsEditQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($obsId: ObservationId!) {
        observation(observationId: $obsId) {
          id
          title
          subtitle
          visualizationTime
          posAngleConstraint {
            constraint
            angle {
              microarcseconds
            }
          }
          targetEnvironment {
            asterism {
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
          constraintSet {
            cloudExtinction
            imageQuality
            skyBackground
            waterVapor
            elevationRange {
              airMass {
                min
                max
              }
              hourAngle {
                minHours
                maxHours
              }
            }
          }
          scienceRequirements {
            mode
            spectroscopy {
              wavelength {
                picometers
              }
              resolution
              signalToNoise
              signalToNoiseAt {
                picometers
              }
              wavelengthCoverage {
                picometers
              }
              focalPlane
              focalPlaneAngle {
                microarcseconds
              }
              capabilities
            }
          }
          scienceMode {
            gmosNorthLongSlit {
              basic {
                grating
                filter
                fpu
              }
              advanced {
                overrideGrating
                overrideFilter
                overrideFpu
                explicitXBin
                explicitYBin
                explicitAmpReadMode
                explicitAmpGain
                explicitRoi
                explicitWavelengthDithers
                explicitSpatialOffsets {
                  microarcseconds
                }
              }
            }
            gmosSouthLongSlit {
              basic {
                grating
                filter
                fpu
              }
              advanced {
                overrideGrating
                overrideFilter
                overrideFpu
                explicitXBin
                explicitYBin
                explicitAmpReadMode
                explicitAmpGain
                explicitRoi
                explicitWavelengthDithers
                explicitSpatialOffsets {
                  microarcseconds
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object Observation {
        type PosAngleConstraint = lucuma.core.model.PosAngleConstraint

        object TargetEnvironment {
          type Asterism = model.TargetWithId
        }
        type ConstraintSet = coreModel.ConstraintSet

        object ScienceRequirements {
          object Spectroscopy {
            type Wavelength         = lucuma.core.math.Wavelength
            type SignalToNoiseAt    = lucuma.core.math.Wavelength
            type WavelengthCoverage = lucuma.core.math.Wavelength
            type FocalPlaneAngle    = lucuma.core.math.Angle
          }
        }

        type ScienceMode = model.ScienceMode
      }
    }

  }

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($obsId: ObservationId!) {
        observationEdit(observationId: $obsId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait EditObservationMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($input: EditObservationInput!){
        editObservation(input: $input) {
          id
        }
      }
    """
  }
}
