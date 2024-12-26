import {Mapper} from "../core/infra/Mapper";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";
import {Allergy} from "../domain/Allergy/Allergy";
import IAllergyDTO from "../dto/IAllergyDTO";
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";
import {ALL} from "node:dns";
import {Code} from "../domain/MedicalCondition/code";
import {Designation} from "../domain/MedicalCondition/designation";
import {Description} from "../domain/MedicalCondition/description";

export class AllergyMap extends Mapper<Allergy> {

  public static toDTO(allergy: Allergy): IAllergyDTO {
    return {
      domainId: allergy.domainId.id.toValue(),
      code: allergy.code,
      designation: allergy.designation,
      description: allergy.description,
      effects: allergy.effects,
      isDeleted: allergy.isDeleted,
    } as IAllergyDTO;
  }

  public static toDomain (allergy: any | Model<IAllergyPersistence & Document> ): Allergy {


    const allergyProps = {
      _id: allergy._id.toString(),
      code: Code.create(allergy.code).getValue(),
      designation: Designation.create(allergy.designation).getValue(),
      description: Description.create(allergy.description).getValue(),
      effects: allergy.effects,
      isDeleted: allergy.isDeleted,
    }

    const allergyOrError = Allergy.create(
      allergyProps,
      new UniqueEntityID(allergy.domainId)
    );

    allergyOrError.isFailure ? console.log(allergyOrError.error) : '';

    return allergyOrError.isSuccess ? allergyOrError.getValue() : null;
  }

  public static toPersistence (allergy: Allergy, id?: Number): IAllergyPersistence {
    if(id === undefined) {
      return {
        domainId: <number> allergy.domainId.id.toValue(),
        code: allergy.code,
        designation: allergy.designation,
        description: allergy.description,
        effects: allergy.effects,
        isDeleted: allergy.isDeleted,
      }
    }

    return {
      domainId: <number> id,
      code: allergy.code,
      designation: allergy.designation,
      description: allergy.description,
      effects: allergy.effects,
      isDeleted: allergy.isDeleted,
    }
  }

}
