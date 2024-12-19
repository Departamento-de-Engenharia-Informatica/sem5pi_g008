import {Mapper} from "../core/infra/Mapper";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";
import {Allergy} from "../domain/Allergy/Allergy";
import IAllergyDTO from "../dto/IAllergyDTO";
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";

export class AllergyMap extends Mapper<Allergy> {

  public static toDTO(allergy: Allergy): IAllergyDTO {
    return {
      domainId: allergy.domainId.id.toValue(),
      allergy : allergy.allergy,
      effect: allergy.effect,
    } as IAllergyDTO;
  }

  public static toDomain (allergy: any | Model<IAllergyPersistence & Document> ): Allergy {

    const allergyOrError = Allergy.create(
      allergy,
      new UniqueEntityID(allergy.domainId)
    );

    allergyOrError.isFailure ? console.log(allergyOrError.error) : '';

    return allergyOrError.isSuccess ? allergyOrError.getValue() : null;
  }

  public static toPersistence (allergy: Allergy, id?: Number): any {
    if(id === undefined) {
      return {
        domainId: allergy.domainId.id.toValue(),
        allergy: allergy.allergy,
        effect: allergy.effect
      }
    }

    return {
      domainId: id,
      allergy: allergy.allergy,
      effect: allergy.effect
    }
  }

}
