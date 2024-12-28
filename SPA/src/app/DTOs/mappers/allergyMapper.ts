import {Allergy} from '../../Domain/Allergy';
import {CreateAllergyDTO} from '../createDTOs/createAllergyDTO';
import {DisplayAllergyDTO} from '../displayDTOs/displayAllergyDTO';
import {BackendAllergyDTO} from '../backendDTOs/backendAllergyDTO';

export class AllergyMapper {

  public static createDtoToDomain(allergy: CreateAllergyDTO): Allergy {

    return {
      domainId: -1,
      code: allergy.allergyCode,
      designation: allergy.allergyDesignation,
      description: allergy.allergyDescription,
      effects: allergy.allergyEffects
    };
  }

  public static backendDtoToDomain(allergy: BackendAllergyDTO): Allergy {

    return {
      domainId: allergy.domainId || -1,
      code: allergy.code,
      designation: allergy.designation,
      description: allergy.description,
      effects: allergy.effects
    };
  }

  public static domainToDisplayDto(allergy: Allergy): DisplayAllergyDTO {

    return {
      domainId: allergy.domainId.toString(),
      code: allergy.code,
      designation: allergy.designation,
      description: allergy.description,
      effects: allergy.effects
    };
  }

  public static domainToBackendDto(allergy: Allergy): BackendAllergyDTO {

    let domainId;

    if (allergy.domainId === -1) {
      domainId = undefined;
    } else {
      domainId = allergy.domainId;
    }

    return {
      domainId: domainId,
      code: allergy.code,
      designation: allergy.designation,
      description: allergy.description,
      effects: allergy.effects
    };
  }
}
