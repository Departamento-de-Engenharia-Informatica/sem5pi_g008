import {MedicalCondition} from '../../Domain/MedicalCondition';
import {DisplayMedicalConditionDTO} from '../displayDTOs/displayMedicalConditionDTO';
import {BackendMedicalConditionDTO} from '../backendDTOs/backendMedicalConditionDTO';
import {MedicalConditionDTO} from '../GenericDTOs/medicalConditionDTO';

export class MedicalConditionMapper {

  public static dtoToDomain(medicalCondition: MedicalConditionDTO): MedicalCondition {

      return {
        code: medicalCondition.code,
        designation: medicalCondition.designation,
        description: medicalCondition.description,
        symptomsList: medicalCondition.symptomsList
      };
  }

  public static domainToDisplayDto(medicalCondition: MedicalCondition): DisplayMedicalConditionDTO {

    return {
      code: medicalCondition.code,
      designation: medicalCondition.designation,
      description: medicalCondition.description,
      symptomsList: medicalCondition.symptomsList
    };
  }

  public static domainToBackendDto(medicalCondition: MedicalCondition): BackendMedicalConditionDTO {

    return {
      code: medicalCondition.code,
      designation: medicalCondition.designation,
      description: medicalCondition.description,
      symptomsList: medicalCondition.symptomsList
    };
  }

}
