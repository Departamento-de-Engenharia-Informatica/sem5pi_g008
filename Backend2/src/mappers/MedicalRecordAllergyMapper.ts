import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {AllergyMap} from "./AllergyMap";
import {IMedicalRecordAllergyPersistence} from "../dataschema/IMedicalRecordAllergyPersistence";

export class MedicalRecordAllergyMapper extends Mapper<MedicalRecordAllergy> {

  public static toDTO(domain: MedicalRecordAllergy): IMedicalRecordAllergyDTO {
    return {
      allergy: AllergyMap.toDTO(domain.allergy),
      doctorId: domain.doctorId,
      comment: domain.comment
    } as IMedicalRecordAllergyDTO;
  }

  public static toDomain (medicalRecordAllergy: any | Model<IMedicalRecordAllergyPersistence & Document> ): MedicalRecordAllergy {

    const medicalRecordAllergyResult = MedicalRecordAllergy.create(
      medicalRecordAllergy,
      new UniqueEntityID(medicalRecordAllergy.domainId)
    );

    medicalRecordAllergyResult.isFailure ? console.log(medicalRecordAllergyResult.error) : '';

    return medicalRecordAllergyResult.isSuccess ? medicalRecordAllergyResult.getValue() : null;
  }

  public static toPersistence (medicalRecordAllergy: MedicalRecordAllergy, id?: Number): any {
    if(id === undefined) {
      return {
        domainId: medicalRecordAllergy.domainId,
        allergy: medicalRecordAllergy.allergy,
        comment: medicalRecordAllergy.comment,
        doctorId: medicalRecordAllergy.doctorId,
      }
    }

    return {
      domainId: id,
      allergy: medicalRecordAllergy.allergy,
      comment: medicalRecordAllergy.comment,
      doctorId: medicalRecordAllergy.doctorId,
    }
  }

}
