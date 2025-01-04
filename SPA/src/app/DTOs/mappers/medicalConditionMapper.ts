import {MedicalCondition} from '../../Domain/MedicalCondition/MedicalCondition';
import {DisplayMedicalConditionDTO} from '../displayDTOs/displayMedicalConditionDTO';
import {BackendMedicalConditionDTO} from '../backendDTOs/backendMedicalConditionDTO';
import {MedicalConditionDTO} from '../GenericDTOs/medicalConditionDTO';

export class MedicalConditionMapper {

  public static dtoToDomain(medicalCondition: MedicalConditionDTO): MedicalCondition {

      return {
        domainId: medicalCondition.domainId,
        code: medicalCondition.code,
        designation: medicalCondition.designation,
        description: medicalCondition.description,
        symptomsList: medicalCondition.symptomsList
      };
  }

  public static domainToDisplayDto(medicalCondition: MedicalCondition): DisplayMedicalConditionDTO {

    return {
      domainId: medicalCondition.domainId,
      code: medicalCondition.code,
      designation: medicalCondition.designation,
      description: medicalCondition.description,
      symptomsList: medicalCondition.symptomsList
    };
  }

  public static domainToBackendDto(medicalCondition: MedicalCondition): BackendMedicalConditionDTO {

    return {
      domainId: medicalCondition.domainId,
      code: medicalCondition.code,
      designation: medicalCondition.designation,
      description: medicalCondition.description,
      symptomsList: medicalCondition.symptomsList
    };
  }

  public static backendDtoToDomain(medicalCondition: BackendMedicalConditionDTO): MedicalCondition {

      return {
        domainId: medicalCondition.domainId,
        code: medicalCondition.code,
        designation: medicalCondition.designation,
        description: medicalCondition.description,
        symptomsList: medicalCondition.symptomsList
      };
    }

}
