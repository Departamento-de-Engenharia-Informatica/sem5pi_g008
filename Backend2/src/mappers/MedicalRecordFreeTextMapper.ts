import {Mapper} from "../core/infra/Mapper";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import IMedicalRecordFreeTextDTO from "../dto/IMedicalRecordFreeTextDTO";

export class MedicalRecordFreeTextMap extends Mapper<MedicalRecordFreeText> {

  public static toDTO(domain: MedicalRecordAllergy): IMedicalRecordFreeTextDTO {
    return {
      doctorId: domain.doctorId,
      comment: domain.comment
    } as IMedicalRecordFreeTextDTO;
  }

  public static toDomain(medicalRecordFreeText: any | Model<IMedicalRecordFreeTextPersistence & Document>): MedicalRecordAllergy {

    const medicalRecordFreeTextResult = MedicalRecordAllergy.create(
      medicalRecordFreeText,
      new UniqueEntityID(medicalRecordFreeText.domainId)
    );

    medicalRecordFreeTextResult.isFailure ? console.log(medicalRecordFreeTextResult.error) : '';

    return medicalRecordFreeTextResult.isSuccess ? medicalRecordFreeTextResult.getValue() : null;
  }

  public static toPersistence(medicalRecordFreeText: MedicalRecordFreeText, id?: Number): any {
    if (id === undefined) {
      return {
        domainId: medicalRecordFreeText.domainId,
        doctorId: medicalRecordFreeText.doctorId,
        comment: medicalRecordFreeText.comment,
      }
    }

    return {
      domainId: id,
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment,
    }
  }

}
