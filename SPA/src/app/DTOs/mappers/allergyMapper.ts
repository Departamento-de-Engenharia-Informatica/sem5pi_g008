import {Allergy} from '../../Domain/Allergy';
import {CreateAllergyDTO} from '../createDTOs/createAllergyDTO';
import {DisplayAllergyDTO} from '../displayDTOs/displayAllergyDTO';
import {BackendAllergyDTO} from '../backendDTOs/backendAllergyDTO';

export class AllergyMapper {

  public static createDtoToDomain(allergy: CreateAllergyDTO): Allergy {

    return {
      domainId: -1,
      allergy: allergy.allergy,
      effect: allergy.effect
    };
  }

  public static domainToDisplayDto(allergy: Allergy): DisplayAllergyDTO {

    let effect = allergy.effect;

    if (effect === undefined) {
      effect = "No effect specified";
    }

    return {
      allergy: allergy.allergy,
      effect: effect
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
      allergy: allergy.allergy,
      effect: allergy.effect
    };
  }
}
