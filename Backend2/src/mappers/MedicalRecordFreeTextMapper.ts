import {Mapper} from "../core/infra/Mapper";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import mongoose, {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";
import {IMedicalRecordFreeTextPersistence} from "../dataschema/IMedicalRecordFreeTextPersistence";

export class MedicalRecordFreeTextMap extends Mapper<MedicalRecordFreeText> {

  public static toDTO(domain: MedicalRecordFreeText): IMedicalRecordFreeTextDTO {
    return {
      doctorId: domain.doctorId,
      domainId: domain.domainId,
      comment: domain.comment
    } as IMedicalRecordFreeTextDTO;
  }

  public static toDomain(medicalRecordFreeText: any | Model<IMedicalRecordFreeTextPersistence & Document>): MedicalRecordFreeText {

    const medicalRecordFreeTextResult = MedicalRecordFreeText.create(
      medicalRecordFreeText,
      new UniqueEntityID(medicalRecordFreeText.domainId)
    );

    medicalRecordFreeTextResult.isFailure ? console.log(medicalRecordFreeTextResult.error) : '';
  
    return medicalRecordFreeTextResult.isSuccess ? medicalRecordFreeTextResult.getValue() : null;
  }

  public static toPersistence(medicalRecordFreeText: MedicalRecordFreeText, id?: number): IMedicalRecordFreeTextPersistence {
    if (id === undefined) {
      return {
        domainId: medicalRecordFreeText.domainId,
        medicalRecordId: new mongoose.Types.ObjectId(medicalRecordFreeText.medicalRecord),
        doctorId: medicalRecordFreeText.doctorId,
        comment: medicalRecordFreeText.comment,
      }
    }

    return {
      domainId: id,
      medicalRecordId: new mongoose.Types.ObjectId(medicalRecordFreeText.medicalRecord),
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment,
    }
  }

}
