import {Mapper} from "../core/infra/Mapper";
import mongoose, {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import IMedicalRecordAllergyDTO from "../dto/IMedicalRecordAllergyDTO";
import {AllergyMap} from "./AllergyMap";
import {IMedicalRecordAllergyPersistence} from "../dataschema/IMedicalRecordAllergyPersistence";

export class MedicalRecordAllergyMapper extends Mapper<MedicalRecordAllergy> {

  public static toDTO(domain: MedicalRecordAllergy): IMedicalRecordAllergyDTO {
    return {
      allergyId: domain.allergy,
      medicalRecordId: domain.medicalRecord,
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

  public static toPersistence(medicalRecordAllergy: MedicalRecordAllergy, id?: number): any {

    return {
      domainId: id !== undefined ? id : medicalRecordAllergy.domainId,
      allergyId: new mongoose.Types.ObjectId(medicalRecordAllergy.allergy),
      medicalRecordId: new mongoose.Types.ObjectId(medicalRecordAllergy.medicalRecord),
      comment: medicalRecordAllergy.comment,
      doctorId: medicalRecordAllergy.doctorId,
    };
  }

}
