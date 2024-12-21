import {Specialization} from '../Domain/Specialization';
import {SpecializationDTO} from '../DTO/SpecializationDTO';

export class SpecializationMap {

  public static toDTO(specialization: Specialization): SpecializationDTO {
    return {
      specializationID: specialization.specializationID,
      specializationName: specialization.specializationName
    } as SpecializationDTO;
  }

  public static toDomain(specializationDTO: SpecializationDTO): Specialization {

    return {
      specializationID: specializationDTO.specializationID,
      specializationName: specializationDTO.specializationName,
      specializationCode: specializationDTO.specializationCode,
      specializationDescription: specializationDTO.specializationDescription
    } as Specialization;

  }
}
